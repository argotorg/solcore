import bare_revert_lib;

function callImportedAbort() returns (unit) {
    bare_revert_lib.abortFromImportedModule();
    return;
}
