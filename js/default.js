document.getElementById('proj-details').style.display = 'none';

document.getElementById('showDiv').onclick=function(){

  var display = document.getElementById('proj-details').style.display;
  if (display === 'none') {
    document.getElementById('proj-details').style.display='';

  } else {
    document.getElementById('proj-details').style.display = 'none';
  }

};
