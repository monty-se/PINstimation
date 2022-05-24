window.onload = function(){ 

let divs = document.getElementsByClassName('page-header');

for (let x = 0; x < divs.length; x++) {
	let div = divs[x];
	let content = div.innerHTML.trim();
  
	if ((content.includes('NA')) ) {
  	div.style.display = 'none';
  }
}

};