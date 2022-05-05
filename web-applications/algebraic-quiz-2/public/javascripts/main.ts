/**
 * Script handling 'main-page.html'.
 */

var quizzes = []; // stores information about quizzes from database
var user = undefined; // stores information whether user is logged or not

/* HTML ELEMENTS USED IN SCRIPT */
const quizTitle = document.getElementById("title");
const quizDescription = document.getElementById("description");
const quizExampleQuestion = document.getElementById("example_question");
const quizExampleAnswer = document.getElementById("example_answer");
const selectQuiz = document.getElementById("select_quiz") as HTMLSelectElement;
const beginButton = document.getElementById("begin") as HTMLButtonElement;
const siteIntro = document.getElementById("intro");

function setExampleQuestion(question) {
    quizExampleQuestion.textContent = "";
    let node = document.createElement("strong");

    node.textContent = "Przykład:";
    quizExampleQuestion.appendChild(node);

    node = document.createElement("p");
    node.textContent = question;

    quizExampleQuestion.appendChild(node);
}

function setExampleAnswer(answer, punishments) {
    quizExampleAnswer.textContent = "";
    let node = document.createElement("strong");

    node.textContent = `Odpowiedź użytkownika: ${answer}`;
    quizExampleAnswer.appendChild(node);

    node = document.createElement("hr");
    quizExampleAnswer.appendChild(node);

    node = document.createElement("p");
    node.textContent = punishments;

    quizExampleAnswer.appendChild(node);
}

/**
 * Function for '<select>' element determining which quiz should be displayed.
 */
function displayQuiz() {
    let changed = false;
    for (let quiz of quizzes) {
        if (quiz.name == selectQuiz.value) {
            quizTitle.textContent = quiz.title;
            quizDescription.textContent = quiz.description;
            setExampleQuestion(quiz.example);
            setExampleAnswer(quiz.example_answer, quiz.example_punishments);
        
            sessionStorage.setItem('quiz', quiz.name);

            if (user.login != undefined) {
                beginButton.setAttribute('onclick', `window.location.href = '/quiz/${quiz.name}';`);
                beginButton.disabled = false;
                console.log(user);
            }

            changed = true;
        }
    }

    /* If selected quiz hasn't been found in database. */ 
    if (!changed) {
        setBlank();
    }
}

function setBlank() {
    quizTitle.textContent = "Tytuł";
    quizDescription.textContent = "Opis quizu.";
    setExampleQuestion("Przykładowe pytanie.");
    setExampleAnswer("", "Kary za błędną odpowiedź.");

    beginButton.setAttribute('onclick', `window.location.href = '/quiz/blank';`);
    beginButton.disabled = true;
}

function createButton(description, type, location) {
    let button = document.createElement("button");
    button.setAttribute("type", "button");
    button.setAttribute("class", `btn btn-${type}`);
    button.setAttribute("onclick", `location.href = '${location}';`);
    button.textContent = description;

    return button;
}

function createUserHandlingDiv(login) {
    let buttonsDiv = document.createElement("div");
    buttonsDiv.setAttribute("id", "buttons_div");

    if (login == undefined) {
        let loginButton = createButton("Zaloguj", "success", "/login");
        loginButton.setAttribute("id", "login_button");
        buttonsDiv.appendChild(loginButton);
    }
    else {
        let welcomeMsg = document.createElement("p");
        welcomeMsg.textContent = "Witaj " + login;
        buttonsDiv.appendChild(welcomeMsg);

        let logoutButton = createButton("Wyloguj", "danger", "/logout");
        logoutButton.style.marginRight = "1rem";
        buttonsDiv.appendChild(logoutButton);

        let changePwd = createButton("Zmień hasło", "primary", "/change-pwd");
        changePwd.setAttribute("id", "change_pwd_button")
        changePwd.style.marginRight = "1rem";
        buttonsDiv.appendChild(changePwd);

        let seeStats = createButton("Zobacz statystyki", "primary", "/stats");
        seeStats.setAttribute("id", "see_stats_button");
        buttonsDiv.appendChild(seeStats);
    }

    return buttonsDiv;
}

function setQuizList(quizzes) {
    for (let quiz of quizzes) {
        let newOption = document.createElement("option");
        newOption.value = quiz.name;
        newOption.textContent = quiz.title;
        selectQuiz.appendChild(newOption);
    }
}

/* MAIN EXECUTION OF SCRIPT */
let userInfoRequest = new XMLHttpRequest();

userInfoRequest.open('GET', '/json/user-info');
userInfoRequest.responseType = 'json';
userInfoRequest.send();

userInfoRequest.onload = function() {
    user = userInfoRequest.response;
    let userView = createUserHandlingDiv(user.login);
    siteIntro.appendChild(userView);
}

let quizzesRequest = new XMLHttpRequest();

quizzesRequest.open('GET', '/json/quiz-list');
quizzesRequest.responseType = 'json';
quizzesRequest.send();

quizzesRequest.onload = function() {
    quizzes = quizzesRequest.response;
    setQuizList(quizzes);
}

setBlank(); // On page load 'blank' quiz is displayed.
selectQuiz.setAttribute('onchange', "displayQuiz()");
