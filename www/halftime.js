shinyjs.halftime = function () {
    const snd = document.getElementById('change_places')
    snd.play();
}
halftime();
// https://community.rstudio.com/t/shinyapps-io-and-sound/19967/3
//https://stackoverflow.com/questions/9419263/how-to-play-audio
//https://stackoverflow.com/questions/879152/how-do-i-make-javascript-beep
//https://stackoverflow.com/questions/53439074/play-tagsaudio-file-until-the-end-in-shiny-app-r
//https://stackoverflow.com/questions/36205419/r-shiny-audio-playback
//https://groups.google.com/g/shiny-discuss/c/zO8hEFCxa0c/m/9B5DlfuVVb8J
//https://community.rstudio.com/t/playing-audio-files-in-shiny-r/77296
//https://stackoverflow.com/questions/31776548/why-cant-javascript-play-audio-files-on-iphone-safari
shinyjs.halftime2 = function () {
  const snd = document.getElementById('change_places')
  snd.src = 'change_places.mp3';
  snd.play();
}
halftime2();