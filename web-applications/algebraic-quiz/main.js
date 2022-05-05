/**
 * Script handling 'main-page.html'.
 */
var jsonString = "{\n    \"asd\": {\n        \"title\": \"Aproksymacje ASD\",\n        \"description\": \"Quiz sk\u0142adaj\u0105cy si\u0119 z pyta\u0144 pojawiaj\u0105cych si\u0119 na cz\u0119\u015Bci testowej egzaminu z przedmiotu Algorytmy i struktury danych (lub im podobnych), prowadzonego na wydziale MIMUW. Jako odpowied\u017A na pytania nale\u017Cy wpisa\u0107 liczb\u0119 ca\u0142kowit\u0105 b\u0119d\u0105c\u0105 przybli\u017Ceniem dok\u0142adnej odpowiedzi. W zale\u017Cno\u015Bci od tego jak bardzo wpisany wynik odbiega od przedzia\u0142u uznawanego za poprawny dolicza si\u0119 odpowiedni\u0105 kar\u0119.\",\n        \"example\": \"Ile razy zd\u0105\u017Cy zmutowa\u0107 wirus z Covid-19 w przeci\u0105gu roku?\",\n        \"example_answer\": 25,\n        \"example_punishments\": \"Poprawny przedzia\u0142: 20-35 Kara +10s: [10-20) U (35-45] Kara +20s: [0; 10) U (45; oo]\",\n        \"type\": 1,\n        \"questions_no\": 4,\n        \"punishments\": [0, 10, 20],\n\n        \"questions\": [\n            {\n                \"question\": \"Wybierasz losow\u0105 stron\u0119 z ksi\u0105\u017Cki \u201CWprowadzenie do algorytm\u00F3w\u201D. Jakie jest prawdopodobie\u0144stwo tego, \u017Ce wybrana przez Ciebie strona jest liczb\u0105 pierwsz\u0105? Odpowied\u017A wyra\u017A w procentach zaokr\u0105glaj\u0105c do cyfr jedno\u015Bci, pomijaj\u0105c znak '%' na ko\u0144cu.\",\n                \"punishments\": [[1, 10], [25, 50]],\n                \"correct_answer\": [10, 25]\n            },\n\n            {\n                \"question\": \"Ka\u017Cdy ze 100 student\u00F3w wybiera losowo i niezale\u017Cnie od pozosta\u0142ych jedn\u0105 stron\u0119 z polskiego wydania ksi\u0105\u017Cki Wprowadzenie do algorytm\u00F3w Cormena, Leisersona, Rivesta i Steina. Jakie jest prawdopodobie\u0144stwo, \u017Ce pewnych dw\u00F3ch student\u00F3w wybra\u0142o t\u0119 sam\u0105 stron\u0119. Odpowied\u017A wyra\u017A w procentach zaokr\u0105glaj\u0105c do cyfr jedno\u015Bci, pomijaj\u0105c znak '%' na ko\u0144cu.\",\n                \"punishments\": [[75, 89], [25, 75]],\n                \"correct_answer\": [89, 100]\n            },\n\n            {\n                \"question\": \"Jaka warto\u015B\u0107 pieni\u0119\u017Cna w monetach jednoz\u0142otowych zmie\u015Bci si\u0119 w standardowej wielko\u015Bci wiadrze?\",\n                \"punishments\": [[1000, 5000], [10000, 50000]],\n                \"correct_answer\": [5000, 10000]\n            },\n\n            {\n                \"question\": \"Ile chococino z automatu Cafe+co na wydziale MIMUW trzeba wypi\u0107 \u017Ceby spe\u0142ni\u0107 dzienne zapotrzebowanie na cukier wskazywane przez GDA?\",\n                \"punishments\": [[4, 6], [9, 11]],\n                \"correct_answer\": [6, 9]\n            }\n        ]\n    },\n\n    \"basic\": {\n        \"title\": \"Dodawanie\",\n        \"description\": \"Klasyczny quiz algebraiczny sprawdzaj\u0105cy umiej\u0119tno\u015B\u0107 dodawania. Jako odpowied\u017A nale\u017Cy poda\u0107 liczb\u0119 ca\u0142kowit\u0105, przy podaniu b\u0142\u0119dnej odpowiedzi naliczana jest 6 sekundowa kara.\",\n        \"example\": \"2 + 2 = ?\",\n        \"example_answer\": 4,\n        \"example_punishments\": \"Kara za b\u0142\u0119dn\u0105 odpowied\u017A: +6s.\",\n        \"type\": 2,\n        \"questions_no\": 6,\n        \"punishments\": [0, 6],\n\n        \"questions\": [\n            {\n                \"question\": \"36 + 11 = ?\",\n                \"punishments\": [],\n                \"correct_answer\": [47]\n            },\n\n            {\n                \"question\": \"46 + 15 = ?\",\n                \"punishments\": [],\n                \"correct_answer\": [61]\n            },\n\n            {\n                \"question\": \"45 + 17 = ?\",\n                \"punishments\": [],\n                \"correct_answer\": [62]\n            },\n\n            {\n                \"question\": \"67 + 8 + 82 = ?\",\n                \"punishments\": [],\n                \"correct_answer\": [157]\n            },\n\n            {\n                \"question\": \"27 + 7 + 23 = ?\",\n                \"punishments\": [],\n                \"correct_answer\": [57]\n            },\n\n            {\n                \"question\": \"93 + 94 + 73 = ?\",\n                \"punishments\": [],\n                \"correct_answer\": [260]\n            }\n        ]\n    }\n}";
var quizzes = JSON.parse(jsonString);
/* HTML ELEMENTS USED IN SCRIPT */
var quizTitle = document.getElementById("title");
var quizDescription = document.getElementById("description");
var quizExampleQuestion = document.getElementById("example_question");
var quizExampleAnswer = document.getElementById("example_answer");
var leaderboardBody = document.getElementById("leaderboard_body");
var selectQuiz = document.getElementById("select_quiz");
var beginButton = document.getElementById("begin");
/* 'GENERIC' FUNCTIONS */
function setExampleQuestion(question) {
    quizExampleQuestion.textContent = "";
    var node = document.createElement("strong");
    node.textContent = "Przykład:";
    quizExampleQuestion.appendChild(node);
    node = document.createElement("p");
    node.textContent = question;
    quizExampleQuestion.appendChild(node);
}
function setExampleAnswer(answer, punishments) {
    quizExampleAnswer.textContent = "";
    var node = document.createElement("strong");
    node.textContent = "Odpowied\u017A u\u017Cytkownika: " + answer;
    quizExampleAnswer.appendChild(node);
    node = document.createElement("hr");
    quizExampleAnswer.appendChild(node);
    node = document.createElement("p");
    node.textContent = punishments;
    quizExampleAnswer.appendChild(node);
}
function setLeaderboard(results) {
    leaderboardBody.textContent = "";
    var x = Math.min(5, results.length);
    results = results.sort(function (n1, n2) { return n1 > n2; });
    for (var i = 0; i < x; i++) {
        var node = createLeaderboardTableRow(i + 1, results[i]);
        leaderboardBody.appendChild(node);
    }
}
function createLeaderboardTableRow(position, result) {
    var node = document.createElement("tr");
    var elem = document.createElement("td");
    elem.textContent = position.toString();
    node.appendChild(elem);
    elem = document.createElement("td");
    elem.textContent = result + "s";
    node.appendChild(elem);
    return node;
}
/**
 * Function for '<select>' element determining which quiz should be displayed.
 */
function displayQuiz() {
    if (selectQuiz.value === "asd") {
        setAsd();
    }
    else if (selectQuiz.value === "basic") {
        setBasic();
    }
    else {
        setBlank();
    }
}
/* 'QUIZ SPECIFIC' FUNCTIONS */
function setAsd() {
    quizTitle.textContent = quizzes.asd.title;
    quizDescription.textContent = quizzes.asd.description;
    setExampleQuestion(quizzes.asd.example);
    setExampleAnswer(quizzes.asd.example_answer, quizzes.asd.example_punishments);
    setLeaderboard(JSON.parse(localStorage.getItem('results'))[0]);
    sessionStorage.setItem('quiz', 'asd');
    beginButton.disabled = false;
}
function setBasic() {
    quizTitle.textContent = quizzes.basic.title;
    quizDescription.textContent = quizzes.basic.description;
    setExampleQuestion(quizzes.basic.example);
    setExampleAnswer(quizzes.basic.example_answer, quizzes.basic.example_punishments);
    setLeaderboard(JSON.parse(localStorage.getItem('results'))[1]);
    sessionStorage.setItem('quiz', 'basic');
    beginButton.disabled = false;
}
function setBlank() {
    quizTitle.textContent = "Tytuł";
    quizDescription.textContent = "Opis quizu.";
    setExampleQuestion("Przykładowe pytanie.");
    setExampleAnswer("", "Kary za błędną odpowiedź.");
    setLeaderboard([]);
    sessionStorage.setItem("quiz", "blank");
    beginButton.disabled = true;
}
/* MAIN EXECUTION OF SCRIPT */
if (localStorage.getItem('results') === null) {
    localStorage.setItem('results', JSON.stringify([[], []]));
}
if (localStorage.getItem('stats') === null) {
    localStorage.setItem('stats', JSON.stringify([[], []]));
}
setBlank(); // On page load 'blank' quiz is displayed.
beginButton.setAttribute('onclick', "window.location.href = 'quiz.html';");
selectQuiz.setAttribute('onchange', "displayQuiz()");
