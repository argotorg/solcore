// Shared storage identity.
export { AppStore, appStore };

enum AppStore { AppStore }

function appStore() returns (AppStore) { 
  return AppStore.AppStore; 
}
