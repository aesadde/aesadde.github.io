document.getElementById('proj-details').style.display = 'none';
document.getElementById('sands-details').style.display = 'none';

document.getElementById('showDiv').onclick=function(){

  var display = document.getElementById('proj-details').style.display;
  if (display === 'none') {
    document.getElementById('proj-details').style.display='';

  } else {
    document.getElementById('proj-details').style.display = 'none';
  }

};

document.getElementById('showDiv-2').onclick=function(){

  var display = document.getElementById('sands-details').style.display;
  if (display === 'none') {
    document.getElementById('sands-details').style.display='';

  } else {
    document.getElementById('sands-details').style.display = 'none';
  }

};
