/**
 * Script handling 'stats.html'.
 */
const statsDiv = document.getElementById("stats_div");
const statsSelect = document.getElementById("select_stats");
function getUserStatsTableHeader() {
    let tr = document.createElement("tr");
    let th = document.createElement("th");
    tr.appendChild(th);
    th = document.createElement("th");
    th.textContent = "Odpowiedź użytkownika";
    tr.appendChild(th);
    th = document.createElement("th");
    th.textContent = "Poprawna odpowiedź";
    tr.appendChild(th);
    th = document.createElement("th");
    th.textContent = "Kara";
    tr.appendChild(th);
    th = document.createElement("th");
    th.textContent = "Średni czas";
    tr.appendChild(th);
    return tr;
}
function fillUserStatsTable(table, stats) {
    let tr;
    let td;
    for (let i = 0; i < stats.averages.length; i++) {
        tr = document.createElement("tr");
        td = document.createElement("td");
        td.textContent = `Pytanie ${i + 1}`;
        tr.appendChild(td);
        td = document.createElement("td");
        td.textContent = stats.usr_answers[i];
        tr.appendChild(td);
        td = document.createElement("td");
        td.textContent = stats.correct_answers[i];
        tr.appendChild(td);
        td = document.createElement("td");
        if (stats.correct_answers[i] != stats.usr_answers[i]) {
            td.textContent = stats.punishment.toString();
        }
        else {
            td.textContent = "0";
        }
        tr.appendChild(td);
        td = document.createElement("td");
        td.textContent = stats.averages[i];
        tr.appendChild(td);
        table.appendChild(tr);
    }
    tr = document.createElement("tr");
    td = document.createElement("td");
    td.textContent = "Całkowity wynik";
    tr.appendChild(td);
    td = document.createElement("td");
    td.setAttribute("colspan", "4");
    td.textContent = stats.full_res;
    tr.appendChild(td);
    table.appendChild(tr);
}
function createTopUsersTable(stats) {
    let topUsersTable = document.createElement("table");
    topUsersTable.width = "100%";
    topUsersTable.setAttribute("class", "table-bordered");
    let tr = document.createElement("tr");
    let th = document.createElement("th");
    th.textContent = "Top 5";
    th.setAttribute("colspan", "3");
    tr.appendChild(th);
    topUsersTable.appendChild(tr);
    let td;
    for (let i = 0; i < stats.topUsers.length; i++) {
        tr = document.createElement("tr");
        td = document.createElement("td");
        td.textContent = (i + 1).toString();
        tr.appendChild(td);
        td = document.createElement("td");
        td.textContent = stats.topUsers[i].user_login;
        tr.appendChild(td);
        td = document.createElement("td");
        td.textContent = (stats.topUsers[i].full_res).toString();
        tr.appendChild(td);
        topUsersTable.appendChild(tr);
    }
    return topUsersTable;
}
function displayStats() {
    statsDiv.textContent = "";
    let statsRequest = new XMLHttpRequest();
    statsRequest.open('GET', `/json/stats/${statsSelect.value}`);
    statsRequest.responseType = 'json'; // now we're getting a string!
    statsRequest.send();
    statsRequest.onload = function () {
        var stats = statsRequest.response; // get the string from the response
        console.log(stats);
        let userStatsTable = document.createElement("table");
        userStatsTable.setAttribute("class", "table-bordered");
        userStatsTable.width = "100%";
        userStatsTable.style.marginBottom = "1rem";
        let tr = getUserStatsTableHeader();
        userStatsTable.appendChild(tr);
        fillUserStatsTable(userStatsTable, stats);
        let topUsersTable = createTopUsersTable(stats);
        statsDiv.appendChild(userStatsTable);
        statsDiv.appendChild(topUsersTable);
    };
}
/* MAIN EXECUTION OF SCRIPT */
let quizzesRequest = new XMLHttpRequest();
quizzesRequest.open('GET', '/json/usr-solved-quizzes');
quizzesRequest.responseType = 'json';
quizzesRequest.send();
quizzesRequest.onload = function () {
    var solvedQuizzes = quizzesRequest.response;
    for (let quiz of solvedQuizzes) {
        let newOption = document.createElement("option");
        newOption.value = quiz.quiz_name;
        newOption.textContent = quiz.title;
        statsSelect.appendChild(newOption);
    }
    statsSelect.setAttribute('onchange', 'displayStats()');
};
