import NumLib;

function adjustBalance(initialBalance, depositAmount, withdrawalAmount,feeAmount) {
    let totalDeposits = depositAmount;
    let totalWithdrawals = withdrawalAmount + feeAmount;
    let balanceAfterDeposit = initialBalance + totalDeposits;
    let netChange = totalDeposits - totalWithdrawals;
    let finalBalance = balanceAfterDeposit - totalWithdrawals;
    let totalChanges = totalDeposits + totalWithdrawals;
    return (finalBalance, totalChanges);
}
