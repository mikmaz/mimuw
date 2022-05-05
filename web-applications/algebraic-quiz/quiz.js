var jsonString = "{\n    \"asd\": {\n        \"title\": \"Aproksymacje ASD\",\n        \"description\": \"Quiz sk\u0142adaj\u0105cy si\u0119 z pyta\u0144 pojawiaj\u0105cych si\u0119 na cz\u0119\u015Bci testowej egzaminu z przedmiotu Algorytmy i struktury danych (lub im podobnych), prowadzonego na wydziale MIMUW. Jako odpowied\u017A na pytania nale\u017Cy wpisa\u0107 liczb\u0119 ca\u0142kowit\u0105 b\u0119d\u0105c\u0105 przybli\u017Ceniem dok\u0142adnej odpowiedzi. W zale\u017Cno\u015Bci od tego jak bardzo wpisany wynik odbiega od przedzia\u0142u uznawanego za poprawny dolicza si\u0119 odpowiedni\u0105 kar\u0119.\",\n        \"example\": \"Ile razy zd\u0105\u017Cy zmutowa\u0107 wirus z Covid-19 w przeci\u0105gu roku?\",\n        \"example_answer\": 25,\n        \"example_punishments\": \"Poprawny przedzia\u0142: 20-35 Kara +10s: [10-20) U (35-45] Kara +20s: [0; 10) U (45; oo]\",\n        \"type\": 1,\n        \"questions_no\": 4,\n        \"punishments\": [0, 10, 20],\n\n        \"questions\": [\n            {\n                \"question\": \"Wybierasz losow\u0105 stron\u0119 z ksi\u0105\u017Cki \u201CWprowadzenie do algorytm\u00F3w\u201D. Jakie jest prawdopodobie\u0144stwo tego, \u017Ce wybrana przez Ciebie strona jest liczb\u0105 pierwsz\u0105? Odpowied\u017A wyra\u017A w procentach zaokr\u0105glaj\u0105c do cyfr jedno\u015Bci, pomijaj\u0105c znak '%' na ko\u0144cu.\",\n                \"punishments\": [[1, 10], [25, 50]],\n                \"correct_answer\": [10, 25]\n            },\n\n            {\n                \"question\": \"Ka\u017Cdy ze 100 student\u00F3w wybiera losowo i niezale\u017Cnie od pozosta\u0142ych jedn\u0105 stron\u0119 z polskiego wydania ksi\u0105\u017Cki Wprowadzenie do algorytm\u00F3w Cormena, Leisersona, Rivesta i Steina. Wska\u017C przedzia\u0142, w kt\u00F3rym znajduje si\u0119 prawdopodobie\u0144stwo zdarzenia, \u017Ce pewnych dw\u00F3ch student\u00F3w wybra\u0142o t\u0119 sam\u0105 stron\u0119. Odpowied\u017A wyra\u017A w procentach zaokr\u0105glaj\u0105c do cyfr jedno\u015Bci, pomijaj\u0105c znak '%' na ko\u0144cu.\",\n                \"punishments\": [[75, 89], [25, 75]],\n                \"correct_answer\": [89, 100]\n            },\n\n            {\n                \"question\": \"Jaka warto\u015B\u0107 pieni\u0119\u017Cna w monetach jednoz\u0142otowych zmie\u015Bci si\u0119 w standardowej wielko\u015Bci wiadrze?\",\n                \"punishments\": [[1000, 5000], [10000, 50000]],\n                \"correct_answer\": [5000, 10000]\n            },\n\n            {\n                \"question\": \"Ile chococino z automatu 'Cafe+co' na wydziale MIMUW trzeba wypi\u0107 \u017Ceby spe\u0142ni\u0107 dzienne zapotrzebowanie na cukier wskazywane przez GDA?\",\n                \"punishments\": [[4, 6], [9, 11]],\n                \"correct_answer\": [6, 9]\n            }\n        ]\n    },\n\n    \"basic\": {\n        \"title\": \"Dodawanie\",\n        \"description\": \"Klasyczny quiz algebraiczny sprawdzaj\u0105cy umiej\u0119tno\u015B\u0107 dodawania. Jako odpowied\u017A nale\u017Cy poda\u0107 liczb\u0119 ca\u0142kowit\u0105, przy podaniu b\u0142\u0119dnej odpowiedzi naliczana jest 6 sekundowa kara.\",\n        \"example\": \"2 + 2 = ?\",\n        \"example_answer\": 4,\n        \"example_punishments\": \"Kara za b\u0142\u0119dn\u0105 odpowied\u017A: +6s.\",\n        \"type\": 2,\n        \"questions_no\": 6,\n        \"punishments\": [0, 6],\n\n        \"questions\": [\n            {\n                \"question\": \"36 + 11 = ?\",\n                \"punishments\": [],\n                \"correct_answer\": [47]\n            },\n\n            {\n                \"question\": \"46 + 15 = ?\",\n                \"punishments\": [],\n                \"correct_answer\": [61]\n            },\n\n            {\n                \"question\": \"45 + 17 = ?\",\n                \"punishments\": [],\n                \"correct_answer\": [62]\n            },\n\n            {\n                \"question\": \"67 + 8 + 82 = ?\",\n                \"punishments\": [],\n                \"correct_answer\": [157]\n            },\n\n            {\n                \"question\": \"27 + 7 + 23 = ?\",\n                \"punishments\": [],\n                \"correct_answer\": [57]\n            },\n\n            {\n                \"question\": \"93 + 94 + 73 = ?\",\n                \"punishments\": [],\n                \"correct_answer\": [260]\n            }\n        ]\n    }\n}";
var quizzes = JSON.parse(jsonString);
/* HTML ELEMENTS USED IN SCRIPT */
var htmlTitle = document.getElementById("title");
var htmlDescription = document.getElementById("description");
var quizProgress = document.getElementById("quiz_progress");
var htmlQuestionNumber = document.getElementById("question_number");
var htmlQuestion = document.getElementById("question");
var answer = document.getElementById("ans");
var nextButton = document.getElementById("next");
var prevButton = document.getElementById("previous");
/* VARIABLES WHICH WILL BE SET ACCORDING TO QUIZ TYPE */
var questions = [];
var questionsNumber = 0;
var quizTitle = "";
var quizDescription = "";
/**
 * Function setting 'quiz-describing' variables according to value set in 'sessionStorage'.
 */
function setQuiz() {
    if (sessionStorage.getItem("quiz") === "asd") {
        quizTitle = quizzes.asd.title;
        quizDescription = quizzes.asd.description;
        questionsNumber = quizzes.asd.questions_no;
        questions = quizzes.asd.questions;
    }
    else if (sessionStorage.getItem("quiz") === "basic") {
        quizTitle = quizzes.basic.title;
        quizDescription = quizzes.basic.description;
        questionsNumber = quizzes.basic.questions_no;
        questions = quizzes.basic.questions;
    }
}
/**
 * Function checking if answers for all questions except last have been provided.
 */
function checkAnswersExceptLast() {
    for (var i = 0; i < questionsNumber - 1; i++) {
        if (answers[i] === "") {
            return false;
        }
    }
    return true;
}
/**
 * Function setting progress bar according to which questions have been answered.
 */
function setProgress() {
    quizProgress.innerHTML = "";
    var width = "width:" + 100 / questionsNumber + "%";
    for (var i = 1; i <= questionsNumber; i++) {
        var node = document.createElement("div");
        var text = document.createTextNode(i.toString());
        node.appendChild(text);
        if (answers[i - 1] === "") {
            node.setAttribute("class", "progress-bar");
        }
        else {
            node.setAttribute("class", "progress-bar bg-success");
        }
        node.setAttribute("role", "progressbar");
        node.style.width = 100 / questionsNumber + "%";
        quizProgress.appendChild(node);
    }
}
/**
 * Function sets page view based on question number (@param k).
 */
function setView(k) {
    htmlQuestionNumber.textContent = "Zadanie " + k.toString();
    htmlQuestion.textContent = (questions[k - 1]).question;
    answer.value = answers[k - 1];
    setProgress();
    if (k === 1) {
        prevButton.disabled = true;
        nextButton.textContent = "Następne pytanie";
        nextButton.setAttribute("onclick", "nextClick()");
        answer.setAttribute("onkeyup", "");
        nextButton.disabled = false;
    }
    else if (k === questionsNumber) {
        prevButton.disabled = false;
        nextButton.textContent = "Zakończ";
        nextButton.setAttribute("onclick", "endQuiz()");
        if (checkAnswersExceptLast() === true) {
            if (answers[questionsNumber - 1] === "") {
                nextButton.disabled = true;
            }
            else {
                nextButton.disabled = false;
            }
            answer.setAttribute("onkeyup", "lastAnswer()");
        }
        else {
            answer.setAttribute("onkeyup", "");
            nextButton.disabled = true;
        }
    }
    else {
        prevButton.disabled = false;
        nextButton.textContent = "Następne pytanie";
        nextButton.setAttribute("onclick", "nextClick()");
        answer.setAttribute("onkeyup", "");
        nextButton.disabled = false;
    }
}
/* FUNCTIONS FOR BUTTONS AND INPUT */
function lastAnswer() {
    if ((/^(0|[1-9][0-9]*)$/).test(answer.value) === true) {
        nextButton.disabled = false;
    }
    else {
        nextButton.disabled = true;
    }
}
function nextClick() {
    var timer = new Date().getTime();
    timers[currentQuestion - 1] += timer - start;
    start = timer;
    if ((/^(0|[1-9][0-9]*)$/).test(answer.value) === true) {
        answers[currentQuestion - 1] = answer.value;
    }
    else {
        answers[currentQuestion - 1] = "";
    }
    currentQuestion += 1;
    setView(currentQuestion);
}
function prevClick() {
    var timer = new Date().getTime();
    timers[currentQuestion - 1] += timer - start;
    start = timer;
    if ((/^(0|[1-9][0-9]*)$/).test(answer.value) === true) {
        answers[currentQuestion - 1] = answer.value;
    }
    else {
        answers[currentQuestion - 1] = "";
    }
    currentQuestion -= 1;
    setView(currentQuestion);
}
function endQuiz() {
    var timer = new Date().getTime();
    timers[currentQuestion - 1] += timer - start;
    answers[currentQuestion - 1] = answer.value;
    sessionStorage.setItem('answers', JSON.stringify(answers));
    sessionStorage.setItem('timers', JSON.stringify(timers));
    window.location.href = 'end-quiz.html';
}
/* MAIN EXECUTION OF SCRIPT */
setQuiz();
var answers = [];
var timers = [];
for (var i = 1; i <= questionsNumber; i++) {
    answers.push("");
    timers.push(0);
}
var currentQuestion = 1;
htmlTitle.textContent = quizTitle;
htmlDescription.textContent = quizDescription;
setView(currentQuestion);
prevButton.setAttribute("onclick", "prevClick()");
var start = new Date().getTime();
