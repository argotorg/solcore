#include "EVMHost.h"

#include <nlohmann/json.hpp>

#include <iostream>
#include <fstream>

using namespace solidity;
using namespace solidity::test;

struct ResultRecorder
{
	explicit ResultRecorder(std::optional<std::string> const& _outfile): m_outfile(_outfile)
	{
	}

	~ResultRecorder()
	{
		if (m_outfile)
		{
			nlohmann::json const j = m_outputContents;
			std::ofstream outputStream {m_outfile.value()};
			outputStream << j;
		}
	}

	void record(
		std::string const& _file,
		std::string const& _message,
		std::string const& _actual,
		std::string const& _desired,
		u256 const& _gasUsed,
		u256 const& _gasUsedForDeposit
	)
	{
		if (m_outfile)
		{
			std::map<std::string, std::string> const status {
				{"message", _message},
				{"actual", _actual},
				{"desired", _desired},
				{"gasUsed", _gasUsed.str()},
				{"gasUsedForDeposit", _gasUsedForDeposit.str()}
			};
			m_outputContents[_file].push_back(status);
		}
	}

	std::optional<std::string> m_outfile;
	std::map<std::string, std::vector<std::map<std::string, std::string>>> m_outputContents;
};

int main(int argc, char** argv)
{
	if (argc != 3 && argc != 4)
	{
		std::cerr << "Usage: " << argv[0] << " <evmone> <testtracefile> [<resultfile>]" << std::endl;
		return 1;
	}

	std::ifstream f(argv[2]);
	nlohmann::json testtrace = nlohmann::json::parse(f);

	evmc::VM& vm = EVMHost::getVM(argv[1]);

	std::optional<std::string> outfile = argc == 4 ? std::make_optional<std::string>(argv[3]) : std::nullopt;
	ResultRecorder resultRecorder{outfile};

	bool hasTestFailure = false;
	for(auto&& [filename, testdata]: testtrace.items())
	{
		std::cout << filename << std::endl;
		std::unique_ptr<EVMHost> evmcHost;

		langutil::EVMVersion const evmVersion{};
		evmcHost = std::make_unique<EVMHost>(evmVersion, vm);

		auto account = [](size_t i) {
			return h160(h256(u256{"0x1212121212121212121212121212120000000012"} + i * 0x1000), h160::AlignRight);
		};

		for (size_t i = 0; i < 10; i++)
			evmcHost->accounts[EVMHost::convertToEVMC(account(i))].balance =
				EVMHost::convertToEVMC(u256(1) << 100);

		bytes bytecode = fromHex(testdata["bytecode"]);
		h160 sender = account(0);
		h160 contractAddress;
		unsigned i = 0;
		for (auto& test: testdata["tests"])
		{
			++i;
			evmcHost->newBlock();
			evmc_message message{};
			bytes input = fromHex(test["input"]["calldata"].get<std::string>());
			message.sender = EVMHost::convertToEVMC(sender);
			message.value = EVMHost::convertToEVMC(u256(test["input"]["value"].get<std::string>()));
			auto kind = test["kind"].get<std::string>();
			if (kind == "constructor")
			{
				input = bytecode + input;
				message.input_data = input.data();
				message.input_size = input.size();

				message.kind = EVMC_CREATE;
				message.recipient = {};
				message.code_address = {};
			}
			else if (kind == "call")
			{
				message.input_data = input.data();
				message.input_size = input.size();
				message.kind = EVMC_CALL;
				message.recipient = EVMHost::convertToEVMC(contractAddress);
				message.code_address = message.recipient;
			}
			else
			{
				std::cerr << "Unrecognized kind: " << kind << std::endl;
				hasTestFailure = true;
				resultRecorder.record(filename, "Unrecognized kind of test", kind, R"("constructor" or "call")", 0, 0);
				continue;
			}

			message.gas = 100000000;

			unsigned const refundRatio = evmVersion >= langutil::EVMVersion::london() ? 5 : 2;
			evmc::Result result = evmcHost->call(message);
			auto const totalGasUsed = message.gas - result.gas_left;
			auto const gasRefund = std::min(u256(result.gas_refund), u256(totalGasUsed) / refundRatio);
			auto const gasUsed = totalGasUsed - gasRefund;
			auto const gasUsedForDeposit = evmcHost->totalCodeDepositGas();

			auto output = bytes(result.output_data, result.output_data + result.output_size);
			if (kind == "constructor")
			{
				contractAddress = EVMHost::convertFromEVMC(result.create_address);
			}

			bool status = result.status_code == EVMC_SUCCESS;

			if (kind == "constructor")
			{
				if (!status)
				{
					std::cerr << "Creation failed." << std::endl;
					resultRecorder.record(filename, "Creation failed for constructor test.", "", "", gasUsed, gasUsedForDeposit);
					hasTestFailure = true;
					continue;
				}
				resultRecorder.record(filename, "Creation succeeded.", "", "", gasUsed, gasUsedForDeposit);
			}
			else
			{
				auto expectedStatus = test["output"]["status"].get<std::string>();
				if (expectedStatus == "failure")
				{
					if (status)
					{
						std::cerr << "Expected failure but got success" << std::endl;
						resultRecorder.record(filename, "Expected test status failure but got success.", "success", expectedStatus, gasUsed, gasUsedForDeposit);
						hasTestFailure = true;
						continue;
					}
				}
				else if (expectedStatus == "success")
				{
					if (!status)
					{
						std::cerr << "Expected success but got failure" << std::endl;
						resultRecorder.record(filename, "Expected test status success but got failure.", "failure", expectedStatus, gasUsed, gasUsedForDeposit);
						hasTestFailure = true;
						continue;
					}
				}
				auto expectedOutput = test["output"]["returndata"].get<std::string>();
				if (output != fromHex(expectedOutput))
				{
					std::cerr << "Expected " << expectedOutput << " but got " << toHex(output) << std::endl;
					resultRecorder.record(filename, "Expected different output.", toHex(output), expectedOutput, gasUsed, gasUsedForDeposit);
					hasTestFailure = true;
					continue;
				}
				resultRecorder.record(filename, "Passed.", toHex(output), expectedOutput, gasUsed, gasUsedForDeposit);
			}
		}
		std::cout << "  => " << i << " tests performed." << std::endl;
	}

	return hasTestFailure ? EXIT_FAILURE : EXIT_SUCCESS;
}
