To run: `testrunner <evmone> <testtracefile> [<resultfile>]`

Example: Running

```bash
testrunner /path/to/libevmone.so example.json example_output.json
```

yields

```json
{
  "semanticTests/viaYul/copy_struct_invalid_ir_bug.sol": [
    {
      "actual": "",
      "desired": "",
      "gasUsed": "228927",
      "gasUsedForDeposit": "160400",
      "message": "Creation succeeded."
    },
    {
      "actual": "",
      "desired": "",
      "gasUsed": "113117",
      "gasUsedForDeposit": "0",
      "message": "Passed."
    }
  ]
}
```
