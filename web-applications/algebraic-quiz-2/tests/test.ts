import { driver, By, Builder, WebDriver, WebElement } from "mocha-webdriver";
import { expect } from "chai";
import * as Faker from "faker";

const host = `http://localhost:3000`;
const delayWaitTimeout = 200;

const logIn = async function (driver: WebDriver, username: string, password: string) {
  //await driver.wait((await driver.find("button[id=login_button]")).isDisplayed(), elementWaitTimeout);
  await (await driver.find("button[id=login_button]")).doClick();
  //await driver.wait((await driver.find("input[id=input_login]")).isDisplayed(), elementWaitTimeout);
  await (await driver.find("input[id=input_login]")).doSendKeys(username);
  await (await driver.find("input[id=input_pwd]")).doSendKeys(password);
  await (await driver.find("button[id=login_user]")).doClick();
};

const changePassword = async function (driver: WebDriver, oldPassword: string, newPassword: string) {
  await (await driver.find("button[id=change_pwd_button]")).doClick();
  await (await driver.find("input[id=change_pwd_input]")).doSendKeys(newPassword);
  await (await driver.find("button[id=submit_new_pwd]")).doClick();
};

const delay = function (ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
};

describe("Tests", function () {
  this.timeout(20000);

  this.beforeEach(async function () {
    await driver.get(host);
  });

  this.afterEach(async function () {
    await driver.manage().deleteAllCookies();
  });

  it("All sessions are being logged out after changing password.", async function () {
    const secondDriver = await new Builder().forBrowser("firefox").build();

    // Login first session.
    await logIn(driver, "user1", "user1");
 
    // Login second session and change password.
    await secondDriver.get(host);
    await delay(delayWaitTimeout);
    await logIn(secondDriver, "user1", "user1");
    await delay(delayWaitTimeout);
    await changePassword(secondDriver, "user1", "test");
 
    // Check if first session is logged out.
    await driver.get(host);
    expect(await (await driver.find("button[id=login_button]")).isDisplayed()).to.be.equal(true);

    // Set back the old password.
    await logIn(secondDriver, "user1", "test");
    await changePassword(secondDriver, "user1", "user1");
 
    await secondDriver.quit();
  });

  it("Stats for quiz are displayed after solving, user can't solve twice the same quiz.", async function () {
    // Login user.
    await delay(delayWaitTimeout);
    await logIn(driver, "user2", "user2");

    // Start quiz.
    await delay(delayWaitTimeout);
    await (await driver.find("option[value=basic]")).doClick();
    await (await driver.find("button[id=begin]")).doClick();
    await delay(delayWaitTimeout);

    const inputAnswer: WebElement = await driver.find("input[id=ans]");
    const nextButton: WebElement = await driver.find("button[id=next]");

    // Solve the quiz.
    for (let i = 1; i <= 6; i++) {
      await delay(delayWaitTimeout);
      await inputAnswer.sendKeys(Faker.random.number());
      await nextButton.doClick();
    }

    // Check if stats for the solved quiz are visible.
    await delay(delayWaitTimeout);
    await (await driver.find("button[id=see_stats_button]")).doClick();
    await delay(delayWaitTimeout);
    expect(await (await driver.find("option[value=basic]")).isDisplayed()).to.be.equal(true);

    // Go back to the main page and check if we can solve again the same quiz.
    await (await driver.find("a[href='/']")).doClick();

    await delay(delayWaitTimeout);
    await (await driver.find("option[value=basic]")).doClick();
    await (await driver.find("button[id=begin]")).doClick();
    
    expect((await driver.getCurrentUrl())).to.equal("http://localhost:3000/error/solved");
  });
});