// Persistence.js
export const openDB = () => {
  console.log("openDB");
  return new Promise((resolve, reject) => {
    const request = indexedDB.open("his-name-o-db", 1);

    request.onerror = () => reject(request.error);

    request.onupgradeneeded = (event) => {
      console.log("onupgradeneeded");
      const db = event.target.result;
      if (!db.objectStoreNames.contains("boards")) {
        console.log("boards database doesn't exist");
        const store = db.createObjectStore("boards", { keyPath: "id", autoIncrement: true });
      } else {
        console.log("boards do exist");
      }
    };

    request.onsuccess = () => {
      console.log("success");
      resolve();
    }
  });
};

export const saveBoard = (board) => {
  console.log(board)
  return new Promise((resolve, reject) => {
    const request = indexedDB.open("his-name-o-db", 1);

    request.onerror = () => reject(request.error);

    request.onsuccess = (event) => {
      const db = event.target.result;
      const transaction = db.transaction("boards", "readwrite");
      const store = transaction.objectStore("boards");
      let saveRequest;

      if(board.id !== 0) {
        console.log("Yes id. Updating")
        saveRequest = store.put(board)
      } else {
        console.log("No id. Adding")
        saveRequest = store.add({
          name: board.name,
          board: board.board
        })
      }

      saveRequest.onsuccess = () => resolve();
      saveRequest.onerror = () => reject(saveRequest.error);
    };
  });
};

export const loadBoards = () => {
  return new Promise((resolve, reject) => {
    const request = indexedDB.open("his-name-o-db", 1);

    request.onerror = () => reject(request.error);

    request.onsuccess = (event) => {
      const db = event.target.result;
      const transaction = db.transaction("boards", "readonly");
      const store = transaction.objectStore("boards");

      const getRequest = store.getAll();

      getRequest.onsuccess = () => {
        resolve(getRequest.result || []);
      };

      getRequest.onerror = () => reject(getRequest.error);
    };
  });
};

export const deleteBoard = (id) => {
  return new Promise((resolve, reject) => {
    const request = indexedDB.open("his-name-o-db");

    request.onerror = () => reject(request.error);

    request.onsuccess = (event) => {
      const db = event.target.result;
      const transaction = db.transaction("boards", "readwrite");
      const store = transaction.objectStore("boards");

      const deleteRequest = store.delete(id);

      deleteRequest.onsuccess = () => resolve();
      deleteRequest.onerror = () => reject(deleteRequest.error)

      transaction.oncomplete = () => db.close();
    };
  });
}
