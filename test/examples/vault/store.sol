// Shared storage identity (see the ERC20 example for the rationale).
export { AppStore, appStore };

enum AppStore { AppStore }

function appStore() returns (AppStore) { 
  return AppStore.AppStore; 
}
