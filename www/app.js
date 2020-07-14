// ADD AND REMOVE PLAYERS FROM FORM
// Initialize variables with all the buttons required for the interactivity
const addPlayerA = document.querySelector('#add-player-a');
const addPlayerB = document.querySelector('#add-player-b');
const a3Group = document.querySelector('#a3-form-group');
const a4Group = document.querySelector('#a4-form-group');
const b3Group = document.querySelector('#b3-form-group');
const b4Group = document.querySelector('#b4-form-group');
const a3Exit = document.querySelector('#a3-x');
const a4Exit = document.querySelector('#a4-x');
const b3Exit = document.querySelector('#b3-x');
const b4Exit = document.querySelector('#b4-x');

/* Initialize a function that accepts:
        p3: Player 3
        p3Close: Player 3 Close Button
        p4: Player 4
        p4Close: Player 4 Close Button
        addBtn: The "Add a Player" button
*/
const extraPlayers = (p3, p3Close, p4, p4Close, addBtn) => {

    // Initialize variables to keep track of each additional player's show state
    let isP3Show = false;
    let isP4Show = false;

    // Listen for clicks on the addBtn
    addBtn.addEventListener('click', () => {
        // If p3 is hidden: show p3 and set its show state to true
        if (isP3Show === false) {
            p3.classList.remove('hidden');
            isP3Show = true;

        // If p3 is already showing: show p4, hide the addBtn, and hide the p3 close button
        } else {
            isP4Show = true;
            p4.classList.remove('hidden');
            addBtn.classList.add('hidden');
            p3Close.classList.add('hidden');
        };
    });

    // Listen for a click on p3Close
    p3Close.addEventListener('click', () => {
        // Hide p3 and set its show state to false
        p3.classList.add('hidden');
        isP3Show = false; 
    });

    // Listen for a click on p4Close
    p4Close.addEventListener('click', () => {
        // Hide p4, set its show state to false, show the addBtn, and show p3Close
        p4.classList.add('hidden');
        isP4Show = false; 
        addBtn.classList.remove('hidden');
        p3Close.classList.remove('hidden');
     });
};

// Use the function on both teams
extraPlayers(a3Group, a3Exit, a4Group, a4Exit, addPlayerA);
extraPlayers(b3Group, b3Exit, b4Group, b4Exit, addPlayerB);
