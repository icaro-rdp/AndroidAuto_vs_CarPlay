let bX;
let bY;
let ball_r = 30;
let tX;
let tY;
let tri_h = 50;
let tri_b = 50;

let FPS = 30;
let joyConnected = false;
let gameStarted = false;
let timeTrialMode = true;
let startFrame;
let elapsedFrames = 0;
let trialDuration = 30 * FPS; //seconds * FPS
let errorList = []

let gamepad;
let videoRecorder;
let capture;

let subjectID;
let platformID;
let taskID;

window.addEventListener("gamepadconnected", (e) => {
  console.log(e);
});

function setup() {
  frameRate(FPS);
  //createCanvas(600, 600);
  createCanvas(window.innerWidth,window.innerHeight);

  capture = createCapture({ video: true, audio: false});
  capture.volume(0);
  capture.hide();
  videoRecorder = new p5.VideoRecorder(capture); //https://github.com/calebfoss/p5.videorecorder
  videoRecorder.onFileReady = saveVideo;

  bX = width/2;
  bY = height/2 - 50;
  tX = width/2;
  tY = height/2 + 50;
}

function draw() {
  noStroke()
  background(30);
  textSize(50);
  textFont('Verdana');
  textAlign(CENTER, CENTER);
  fill(230);

  image(capture, 0, 0, 320, 240);
  
  gamepad = assignGamepadById('Joy-Con L+R');
  
  if(!gamepad){
    text("Connect gamepads",width/2,height/2-200);
    text("Move the stick",width/2,height/2-100);
  }
  else if(!gameStarted){
    text("press the LZ button to start",width/2,height/2-200)
    drawTriangle(false);
    drawBall(false);
    if(gamepad.buttons[6].pressed){
      gameStarted=true;
      startFrame = frameCount;
      startRecording() //start recording video
    }
  }
  else if(!shouldGameStop()) {
    elapsedFrames += 1;
    drawTriangle(true);
    drawBall(true);
    measureError();
  }
  else{
    stopRecording(); //stop recording video\\ 
    saveStatsFile();
    console.log("mean error "+ avgArr(errorList))
    text("done",width/2,height/2-200);
    noLoop(); 
  }
}

function shouldGameStop(){
  if(gamepad.buttons[7].pressed){
    return true;  
  }
  if(timeTrialMode){
    if(elapsedFrames < trialDuration){
      return false;
    }
    return true;
  }
  //if() button or spacebar is pressed
}

function measureError(){
  error = abs(tX-bX);
  errorList.push(error);
}

function drawTriangle(move){
  if(move){
    let speed = 1/75 * width;
    let [xIn, yIn] = gamepad.axes;
    tX = tX + speed*(xIn);
    tX = constrain(tX, 0, width);
  }
  fill(0, 255, 255)
  triangle(tX-tri_b/2,tY , tX, tY - tri_h, tX+tri_b/2, tY);
  
}

function drawBall(move){
  staticTime = 1 * FPS; //seconds standing still at start
  if(move && (elapsedFrames>staticTime)){
    bX = complexCurve(elapsedFrames-staticTime)  
  }

  fill(255, 0, 255)
  circle(bX,bY,ball_r);
}

function drawTrace(){
  for(i=0;i<20;i++){
    let x = complexCurve(frameCount-startFrame + i*5)
    fill(255, 0, 255)
    circle(x,bY-15*i,10);
  }

}

function complexCurve(t){
  t = t/40; //scaling
  hz1 = 0.07;
  hz2 = 0.17;
  hz3 = 0.23;
  a = sin(2*PI*hz1*t)
  b = sin(2*PI*hz2*t)
  c = sin(2*PI*hz3*t)
  y = (a+b+c)/3;
  return y*width/2+(width/2);
}

function assignGamepadById(gamepadId) {
  const gamepads = navigator.getGamepads();
  console.log(gamepads);
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

function avgArr(a){
  sum = a.reduce((total, number) => total + number, 0);
  return sum / a.length;
}

function startRecording() {
  videoRecorder.start();
}

function stopRecording() {
  videoRecorder.stop()
}

function saveVideo() {
  let filename = subjectID + '_' + platformID + '_' + taskID;
  videoRecorder.save(filename);
}

//format: 
//line1: subjectID platformID taskID
//line2: timeOnTask meanAbsError
//line3: list of absError per frame (comma separated)
function saveStatsFile() { 
  let filename = subjectID + '_' + platformID + '_' + taskID+'.txt';
  let l1 = subjectID + ' ' + platformID + ' ' + taskID;
  let l2 = (elapsedFrames/FPS) + ' ' + avgArr(errorList);
  let l3 = errorList.toString();
  let text = l1 + '\n' + l2 + '\n' + l3;
  const blob = new Blob([text], { type: 'text/plain' });
  const a = document.createElement('a');
  a.href = URL.createObjectURL(blob);
  a.download = filename;
  document.body.appendChild(a);
  a.click();
  document.body.removeChild(a);
}