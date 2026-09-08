import {lt} from std;
export { ltproxy };

function ltproxy() returns (bool) {
    let zero : word = 0;
    return (zero < 42);
}
