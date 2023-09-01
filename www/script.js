window.addEventListener("load", function(){

let stringCheck = "1:1..10,000,000";

let browserOutput = document.getElementById("browserOutput");

let tester = document.getElementById("tester");

let updateButton = document.getElementById("update");

let isLoaded = false;

let chrArr = ["chr1", "chr2", "chr3", "chr4", "chr5",
              "chr6", "chr7", "chr8", "chr9", "chr10",
              "chr11", "chr12", "chr13", "chr14",
              "chr15", "chr16", "chr17", "chr18",
              "chr19", "chr20", "chr21", "chr22",
              "chrX", "chrY"];

const interv = setInterval(function() {
if(browserOutput.firstChild != null)
  {
    updateButton.addEventListener("click", updateVals);
    clearInterval(interv);
  }
}, 1000);


function updateVals(){
  console.log("hi");
  let browserInput = document.getElementById("refNameAutocomplete-linearGenomeView");
  //if(browserInput.value == stringCheck){
    const colonSplit = browserInput.value.split(":");
    let chrStart = colonSplit[0];
    let chrFinal = "chr" + chrStart;
    const dotSplit = colonSplit[1].split("..");
    const startSplit = dotSplit[0].split(",");
    const endSplit = dotSplit[1].split(",");
    let startFinal = startSplit[0];
    let endFinal = endSplit[0];
    let i = 1;
    while(startSplit[i] != null)
    {
      startFinal = startFinal.concat(startSplit[i]);
      i++;
    }
    i = 1;
    while(endSplit[i] != null)
    {
      endFinal = endFinal.concat(endSplit[i]);
      i++;
    }
    if(chrArr.includes(chrFinal))
    {
      Shiny.setInputValue("selectChr", chrFinal);
      Shiny.setInputValue("jsStartInput", startFinal);
      Shiny.setInputValue("jsEndInput", endFinal);
      document.getElementById("jsEndInput").value = endFinal;
      document.getElementById("jsStartInput").value = startFinal;
    }
    else
    {
      Shiny.setInputValue("selectChr", "Other");
    }
    
    //stringCheck = browserInput.value;
  //}
}

let genomePlot = document.getElementById("genomePlot");

let sidePanel = document.getElementsByClassName("chartOptions");
let optionsCount = 0;
//console.log(sidePanel[0].firstChild);
sidePanel[0].childNodes[1].addEventListener("click", function(){
  if(optionsCount % 2 == 1){
    this.parentNode.style.width = "92px";
    this.parentNode.style.height = "34px";
    this.innerHTML = "Open Side";
    genomePlot.style.width = "100%";
    this.parentNode.style.overflow = "hidden";
  } else {
    this.parentNode.style.width = "25%";
    this.parentNode.style.height = "100%";
    this.innerHTML = "Close Side";
    genomePlot.style.width = "75%";
    this.parentNode.style.overflow = "visible";
  }
  
  optionsCount++;
});

});