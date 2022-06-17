window.onload = function(){
	$('#downloadButton').click(function (){
		var element = document.getElementById('panelReportMainPanel');

		var opt = {
		  margin:       1,
		  filename:     'myfile.pdf',
		  image:        { type: 'jpeg', quality: 0.98 },
		  html2canvas:  { scale: 8},
		  jsPDF:        { unit: 'in', format: 'letter', orientation: 'landscape' }
		};
		html2pdf(element, opt);
	});

};