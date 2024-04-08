let bX;
let bY;
let ball_r = 60;
let tX;
let tY;
let tri_h = 80;
let tri_b = 80;

let FPS = 30;
let joyConnected = false;
let gameStarted = false;
let timeTrialMode = false;
let startFrame;
let elapsedFrames = 0;
let trialDuration = 45 * FPS; //seconds * FPS
let errorList = [];

let gamepad;
let videoRecorder;
let capture;
let doneButton;

let subjectID;
let platformID;
let taskID;

window.addEventListener("gamepadconnected", (e) => {
  console.log(e);
});

function setup() {
  capture = createCapture({ video: true, audio: false });
  capture.volume(0);
  capture.hide();

  saveEmptyFile(); //to trigger allow download prompt
  saveEmptyFile();

  frameRate(FPS);
  createCanvas(window.innerWidth, window.innerHeight);

  subjectID = window.prompt("Subject ID");
  init();

  doneButton = createButton("Go again");
  doneButton.position(width / 2 - 50, height / 2);
  doneButton.mousePressed(playAgain);
  doneButton.size(100, 50);
  doneButton.hide();
}

function draw() {
  noStroke();
  background(30);
  textSize(50);
  textFont("Verdana");
  textAlign(CENTER, CENTER);
  fill(230);

  image(capture, 0, 0, 320, 240);

  gamepad = assignGamepadById("Joy-Con L+R");

  if (!gamepad) {
    text("Connect gamepads", width / 2, height / 2 - 200);
    text("Move the stick", width / 2, height / 2 - 100);
  } else if (!gameStarted) {
    text("press the LZ button to start", width / 2, height / 2 + 100);
    drawTriangle(false);
    drawBall(false);
    if (gamepad.buttons[6].pressed) {
      gameStarted = true;
      startFrame = frameCount;
      startRecording(); //start recording video
    }
  } else if (!shouldGameStop()) {
    elapsedFrames += 1;
    drawTriangle(true);
    drawBall(true);
    measureError();
  } else {
    stopRecording(); //stop recording video
    saveStatsFileJson();
    console.log("mean error " + avgArr(errorList));
    text("Done", width / 2, height / 2 - 200);
    doneButton.show();
    noLoop();
  }
}

function playAgain() {
  doneButton.hide();
  init();
  loop();
}

function init() {
  elapsedFrames = 0;
  errorList = [];
  joyConnected = false;
  gameStarted = false;
  timeTrialMode = false;

  bX = width / 2;
  bY = height / 5;
  tX = width / 2;
  tY = height / 5 + 130;

  platIn = "NA";
  while (platIn == "NA") {
    platIn = window.prompt("Platform: none/and/app");
    if (platIn.toLowerCase() == "and") {
      platformID = "AA";
    } else if (platIn.toLowerCase() == "app") {
      platformID = "CP";
    } else if (platIn.toLowerCase() == "none") {
      platformID = "NO";
    } else {
      alert("!!! Platfrom unknown: " + platIn);
      platIn = "NA";
    }
  }

  taskin = "na";
  if (platformID == "NO") {
    while (taskin == "na") {
      taskin = window.prompt("task: test/con");
      if (taskin.toLowerCase() == "con") {
        taskID = "t0";
      } else if (taskin.toLowerCase() == "test") {
        taskID = "te";
      } else {
        alert("!!! task unknown: " + taskin);
        taskin = "na";
      }
    }
  } else {
    while (taskin == "na") {
      taskin = window.prompt("task: nav/mus/call");
      if (taskin.toLowerCase() == "nav") {
        taskID = "t1";
      } else if (taskin.toLowerCase() == "mus") {
        taskID = "t2";
      } else if (taskin.toLowerCase() == "call") {
        taskID = "t3";
      } else {
        alert("!!! task unknown: " + taskin);
        taskin = "na";
      }
    }
  }

  if (platformID == "NO") {
    timeTrialMode = true;
  } else {
    timeTrialMode = false;
  }

  videoRecorder = new p5.VideoRecorder(capture); //https://github.com/calebfoss/p5.videorecorder
  videoRecorder.onFileReady = saveVideo;
}

function shouldGameStop() {
  if (gamepad.buttons[7].pressed) {
    return true;
  }
  if (timeTrialMode) {
    if (elapsedFrames < trialDuration) {
      return false;
    }
    return true;
  }
}

function measureError() {
  error = abs(tX - bX);
  errorList.push(error);
}

function drawTriangle(move) {
  if (move) {
    let speed = (1 / 100) * width;
    let [xIn, yIn] = gamepad.axes;
    tX = tX + speed * xIn;
    tX = constrain(tX, 0, width);
  }
  fill(0, 255, 255);
  triangle(tX - tri_b / 2, tY, tX, tY - tri_h, tX + tri_b / 2, tY);
}

function drawBall(move) {
  staticTime = 1 * FPS; //seconds standing still at start
  if (move && elapsedFrames > staticTime) {
    bX = complexCurve(elapsedFrames - staticTime);
  }

  fill(255, 0, 255);
  circle(bX, bY, ball_r);
}

function drawTrace() {
  for (i = 0; i < 20; i++) {
    let x = complexCurve(frameCount - startFrame + i * 5);
    fill(255, 0, 255);
    circle(x, bY - 15 * i, 10);
  }
}

function complexCurve(t) {
  t = t / 40; //scaling
  hz1 = 0.07;
  hz2 = 0.17;
  hz3 = 0.23;
  a = sin(2 * PI * hz1 * t);
  b = sin(2 * PI * hz2 * t);
  c = sin(2 * PI * hz3 * t);
  y = (a + b + c) / 3;
  return (y * width) / 2 + width / 2;
}

function assignGamepadById(gamepadId) {
  const gamepads = navigator.getGamepads();
  for (let i = 0; i < gamepads.length; i++) {
    const gamepad = gamepads[i];
    if (gamepad && gamepad.id.includes(gamepadId)) {
      //console.log('Gamepad found:', gamepad);
      return gamepad;
    }
  }
  //console.log('Gamepad not found:', gamepadId);
  return null;
}

function avgArr(a) {
  sum = a.reduce((total, number) => total + number, 0);
  return sum / a.length;
}

function startRecording() {
  videoRecorder.start();
}

function stopRecording() {
  videoRecorder.stop();
}

function saveVideo() {
  let filename = subjectID + "_" + platformID + "_" + taskID;
  videoRecorder.save(filename);
}

//format:
//line1: subjectID platformID taskID
//line2: timeOnTask meanAbsError
//line3: list of absError per frame (comma separated)
function saveStatsFile() {
  let filename = subjectID + "_" + platformID + "_" + taskID + ".txt";
  let l1 = subjectID + " " + platformID + " " + taskID;
  let l2 = elapsedFrames / FPS + " " + avgArr(errorList);
  let l3 = errorList.toString();
  let text = l1 + "\n" + l2 + "\n" + l3;
  const blob = new Blob([text], { type: "text/plain" });
  const a = document.createElement("a");
  a.href = URL.createObjectURL(blob);
  a.download = filename;
  document.body.appendChild(a);
  a.click();
  document.body.removeChild(a);
}

function saveStatsFileJson() {
  let filename = subjectID + "_" + platformID + "_" + taskID + ".json";
  let data = {
    subjectID: subjectID,
    platformID: platformID,
    taskID: taskID,
    errors_by_frame: errorList,
  };
  let text = JSON.stringify(data);
  const blob = new Blob([text], { type: "application/json" });
  const a = document.createElement("a");
  a.href = URL.createObjectURL(blob);
  a.download = filename;
  document.body.appendChild(a);
  a.click();
  document.body.removeChild(a);
}

function saveEmptyFile() {
  let filename = "000000empty.txt";
  let text = "just an empty file";
  const blob = new Blob([text], { type: "text/plain" });
  const a = document.createElement("a");
  a.href = URL.createObjectURL(blob);
  a.download = filename;
  document.body.appendChild(a);
  a.click();
  document.body.removeChild(a);
}

