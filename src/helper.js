var runningElmModule = Elm.fullscreen(Elm.Renderer);
function download(str){
  window.location = 'http://localhost:3000/download/'+str;
}
function testString(val){
  alert('hey')
  alert(val)
}   
function testRegular(val){
  alert(val)
}                                 
                                                                                    
runningElmModule.ports.conn.subscribe(download)

runningElmModule.ports.tStr.subscribe(testString)
runningElmModule.ports.tRe.subscribe(testRegular)

