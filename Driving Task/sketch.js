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
let startFrame;
let elapsedFrames = 0;
let trialDuration = 30 * FPS; //seconds * FPS
let errorList = []
let gamepad;
let videoRecorder;
let capture;

window.addEventListener("gamepadconnected", (e) => {
  console.log(e);
});

function setup() {
  frameRate(FPS);
  createCanvas(600, 600);

  capture = createCapture({ video: true, audio: false});
  //capture.size(320, 240);
  capture.volume(0);
  capture.hide();
  videoRecorder = new p5.VideoRecorder(capture); //https://github.com/calebfoss/p5.videorecorder
  videoRecorder.onFileReady = showAndSaveVideo;

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
  
  gamepad = navigator.getGamepads()[0];
  
  if(!joyConnected){
    text("Move the stick",width/2,height/2-100)
    if(gamepad!=null){
      joyConnected=true;

    }
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
  else if(elapsedFrames < trialDuration) {
    elapsedFrames += 1;
    drawTriangle(true);
    drawBall(true);
    //drawTrace();
    measureError();
  }
  else{
    stopRecording(); //stop recording video\\ 
    console.log("mean error "+ avgArr(errorList))
    noLoop(); 
  }
}

function measureError(){
  error = abs(tX-bX);
  errorList.push(error);
}

function drawTriangle(move){
  if(move){
    let speed = 8;
    let [xIn, yIn] = gamepad.axes;
    tX = tX + speed*(-yIn);
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

function showAndSaveVideo() {
  //  Get url of recorded video
//  let videoURL = videoRecorder.url;
  //  Create video player element with recording as source
 // let vid = createVideo(videoURL);
  //vid.showControls();
  //  Download the recording
  videoRecorder.save("myVideo");
}