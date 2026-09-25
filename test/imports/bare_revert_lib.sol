export { abortFromImportedModule };

function abortFromImportedModule() returns (unit) {
    revert;
    return;
}
