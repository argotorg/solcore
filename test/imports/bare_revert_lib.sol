export { abortFromImportedModule };

function abortFromImportedModule() returns (()) {
    revert;
    return;
}
