<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
 <head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
    <title>criterion report</title>
    <script language="javascript" type="text/javascript">
      {{#include}}js/jquery-2.1.1.min.js{{/include}}
    </script>
    <script language="javascript" type="text/javascript">
      {{#include}}js/jquery.flot-0.8.3.min.js{{/include}}
    </script>
    <script language="javascript" type="text/javascript">
      {{#include}}js/jquery.criterion.js{{/include}}
    </script>
    <style type="text/css">
{{#include}}criterion.css{{/include}}
    </style>
    <!--[if !IE 7]>
	    <style type="text/css">
		    #wrap {display:table;height:100%}
	    </style>
    <![endif]-->
 </head>
    <body>
     <div id="wrap">
      <div id="main" class="body">
    <h1>criterion performance measurements</h1>

<h2>overview</h2>

<p><a href="#grokularation">want to understand this report?</a></p>

<div id="overview" class="ovchart" style="width:900px;height:100px;"></div>

{{#report}}
<h2><a name="b{{number}}">{{name}}</a></h2>
 <table width="100%">
  <tbody>
   <tr>
    <td><div id="kde{{number}}" class="kdechart"
             style="width:450px;height:278px;"></div></td>
    <td><div id="time{{number}}" class="timechart"
             style="width:450px;height:278px;"></div></td>
<!--
    <td><div id="cycle{{number}}" class="cyclechart"
             style="width:300px;height:278px;"></div></td>
-->
   </tr>
  </tbody>
 </table>

 <table>
  <thead class="analysis">
   <th></th>
   <th class="cibound"
       title="{{anMean.estConfidenceLevel}} confidence level">lower bound</th>
   <th>estimate</th>
   <th class="cibound"
       title="{{anMean.estConfidenceLevel}} confidence level">upper bound</th>
  </thead>
  <tbody>
   <tr>
    <td>OLS regression</td>
    <td><span class="confinterval olstimelb{{number}}">xxx</span></td>
    <td><span class="olstimept{{number}}">xxx</span></td>
    <td><span class="confinterval olstimeub{{number}}">xxx</span></td>
   </tr>
   <tr>
    <td>R&#xb2; goodness-of-fit</td>
    <td><span class="confinterval olsr2lb{{number}}">xxx</span></td>
    <td><span class="olsr2pt{{number}}">xxx</span></td>
    <td><span class="confinterval olsr2ub{{number}}">xxx</span></td>
   </tr>
   <tr>
    <td>Mean execution time</td>
    <td><span class="confinterval citime">{{anMean.estLowerBound}}</span></td>
    <td><span class="time">{{anMean.estPoint}}</span></td>
    <td><span class="confinterval citime">{{anMean.estUpperBound}}</span></td>
   </tr>
   <tr>
    <td>Standard deviation</td>
    <td><span class="confinterval citime">{{anStdDev.estLowerBound}}</span></td>
    <td><span class="time">{{anStdDev.estPoint}}</span></td>
    <td><span class="confinterval citime">{{anStdDev.estUpperBound}}</span></td>
   </tr>
  </tbody>
 </table>

 <span class="outliers">
   <p>Outlying measurements have {{anOutlierVar.ovDesc}}
     (<span class="percent">{{anOutlierVar.ovFraction}}</span>%)
     effect on estimated standard deviation.</p>
 </span>
{{/report}}

 <h2><a name="grokularation">understanding this report</a></h2>

 <p>In this report, each function benchmarked by criterion is assigned
   a section of its own.  The charts in each section are active; if
   you hover your mouse over data points and annotations, you will see
   more details.</p>

 <ul>
   <li>The chart on the left is a
     <a href="http://en.wikipedia.org/wiki/Kernel_density_estimation">kernel
       density estimate</a> (also known as a KDE) of time
     measurements.  This graphs the probability of any given time
     measurement occurring.  A spike indicates that a measurement of a
     particular time occurred; its height indicates how often that
     measurement was repeated.</li>

   <li>The chart on the right is the raw data from which the kernel
     density estimate is built.  The <i>x</i> axis indicates the
     number of loop iterations, while the <i>y</i> axis shows measured
     execution time for the given number of loop iterations.  The
     line behind the values is the linear regression prediction of
     execution time for a given number of iterations. Ideally, all
     measurements will be on (or very near) this line.</li>
 </ul>

 <p>Under the charts is a small table.
   The first two rows are the results of a linear regression run
     on the measurements displayed in the right-hand chart.</p>

 <ul>
   <li><i>OLS regression</i> indicates the
     time estimated for a single loop iteration using an ordinary
     least-squares regression model.  This number is more accurate
     than the <i>mean</i> estimate below it, as it more effectively
     eliminates measurement overhead and other constant factors.</li>
   <li><i>R&#xb2; goodness-of-fit</i> is a measure of how
     accurately the linear regression model fits the observed
     measurements.  If the measurements are not too noisy, R&#xb2;
     should lie between 0.99 and 1, indicating an excellent fit. If
     the number is below 0.99, something is confounding the accuracy
     of the linear model.</li>
   <li><i>Mean execution time</i> and <i>standard deviation</i> are
     statistics calculated from execution time
     divided by number of iterations.</li>
 </ul>

 <p>We use a statistical technique called
   the <a href="http://en.wikipedia.org/wiki/Bootstrapping_(statistics)">bootstrap</a>
   to provide confidence intervals on our estimates.  The
   bootstrap-derived upper and lower bounds on estimates let you see
   how accurate we believe those estimates to be.  (Hover the mouse
   over the table headers to see the confidence levels.)</p>

 <p>A noisy benchmarking environment can cause some or many
   measurements to fall far from the mean.  These outlying
   measurements can have a significant inflationary effect on the
   estimate of the standard deviation.  We calculate and display an
   estimate of the extent to which the standard deviation has been
   inflated by outliers.</p>

<script type="text/javascript">
$(function () {
  function mangulate(rpt) {
    var measured = function(key) {
      var idx = rpt.reportKeys.indexOf(key);
      return rpt.reportMeasured.map(function(r) { return r[idx]; });
    };
    var number = rpt.reportNumber;
    var name = rpt.reportName;
    var mean = rpt.reportAnalysis.anMean.estPoint;
    var iters = measured("iters");
    var times = measured("time");
    var kdetimes = rpt.reportKDEs[0].kdeValues;
    var kdepdf = rpt.reportKDEs[0].kdePDF;

    var meanSecs = mean;
    var units = $.timeUnits(mean);
    var rgrs = rpt.reportAnalysis.anRegress[0];
    var scale = units[0];
    var olsTime = rgrs.regCoeffs.iters;
    $(".olstimept" + number).text(function() {
        return $.renderTime(olsTime.estPoint);
      });
    $(".olstimelb" + number).text(function() {
        return $.renderTime(olsTime.estLowerBound);
      });
    $(".olstimeub" + number).text(function() {
        return $.renderTime(olsTime.estUpperBound);
      });
    $(".olsr2pt" + number).text(function() {
        return rgrs.regRSquare.estPoint.toFixed(3);
      });
    $(".olsr2lb" + number).text(function() {
        return rgrs.regRSquare.estLowerBound.toFixed(3);
      });
    $(".olsr2ub" + number).text(function() {
        return rgrs.regRSquare.estUpperBound.toFixed(3);
      });
    mean *= scale;
    kdetimes = $.scaleBy(scale, kdetimes);
    var kq = $("#kde" + number);
    var k = $.plot(kq,
           [{ label: name + " time densities",
              data: $.zip(kdetimes, kdepdf),
              }],
           { xaxis: { tickFormatter: $.unitFormatter(scale) },
             yaxis: { ticks: false },
             grid: { borderColor: "#777",
                     hoverable: true, markings: [ { color: '#6fd3fb',
                     lineWidth: 1.5, xaxis: { from: mean, to: mean } } ] },
           });
    var o = k.pointOffset({ x: mean, y: 0});
    kq.append('<div class="meanlegend" title="' + $.renderTime(meanSecs) +
              '" style="position:absolute;left:' + (o.left + 4) +
              'px;bottom:139px;">mean</div>');
    $.addTooltip("#kde" + number,
                 function(secs) { return $.renderTime(secs / scale); });
    var timepairs = new Array(times.length);
    var lastiter = iters[iters.length-1];
    var olspairs = [[0,0], [lastiter, lastiter * scale * olsTime.estPoint]];
    for (var i = 0; i < times.length; i++)
      timepairs[i] = [iters[i],times[i]*scale];
    iterFormatter = function() {
      var denom = 0;
      return function(iters) {
	if (iters == 0)
          return '';
	if (denom > 0)
	  return (iters / denom).toFixed();
        var power;
	if (iters >= 1e9) {
	    denom = '1e9'; power = '&#x2079;';
        }
	if (iters >= 1e6) {
	    denom = '1e6'; power = '&#x2076;';
        }
        else if (iters >= 1e3) {
            denom = '1e3'; power = '&#xb3;';
        }
        else denom = 1;
        if (denom > 1) {
          iters = (iters / denom).toFixed();
	  iters += '&times;10' + power + ' iters';
        } else {
          iters += ' iters';
        }
        return iters;
      };
    };
    $.plot($("#time" + number),
           [{ label: "regression", data: olspairs,
              lines: { show: true } },
            { label: name + " times", data: timepairs,
              points: { show: true } }],
            { grid: { borderColor: "#777", hoverable: true },
              xaxis: { tickFormatter: iterFormatter() },
              yaxis: { tickFormatter: $.unitFormatter(scale) } });
    $.addTooltip("#time" + number,
		 function(iters,secs) {
		   return ($.renderTime(secs / scale) + ' / ' +
			   iters.toLocaleString() + ' iters');
		 });
    if (0) {
      var cyclepairs = new Array(cycles.length);
      for (var i = 0; i < cycles.length; i++)
	cyclepairs[i] = [cycles[i],i];
      $.plot($("#cycle" + number),
	     [{ label: name + " cycles",
		data: cyclepairs }],
	     { points: { show: true },
	       grid: { borderColor: "#777", hoverable: true },
	       xaxis: { tickFormatter:
			function(cycles,axis) { return cycles + ' cycles'; }},
	       yaxis: { ticks: false },
	     });
      $.addTooltip("#cycles" + number, function(x,y) { return x + ' cycles'; });
    }
  };
  var reports = {{json}};
  reports.map(mangulate);

  var benches = [{{#report}}"{{name}}",{{/report}}];
  var ylabels = [{{#report}}[-{{number}},'<a href="#b{{number}}">{{name}}</a>'],{{/report}}];
  var meansUnscaled = [{{#report}}{{anMean.estPoint}},{{/report}}];
  var xs = [];

  if (!String.prototype.startsWith) {
    Object.defineProperty(String.prototype, 'startsWith', {
        enumerable: false,
        configurable: false,
        writable: false,
        value: function (searchString, position) {
            position = position || 0;
             return this.indexOf(searchString, position) === position;
        }
    });
}

// Find the prefixes first
var _prefixes = {}, prefixes = [];
for (var i in benches)
    _prefixes[benches[i].split('/')[0]] = true;
for (var prefix in _prefixes)
    prefixes.push(prefix);


 // console.log(prefixes);
var data = {};
for (var p in prefixes) {
    console.log(p);
    var prefix = prefixes[p];
    data[prefix] = {benches:[], ylabels:[], means:[]};
    for (var i in benches) {
        if (benches[i].startsWith(prefix)) {
            data[prefix].benches.push(benches[i]);
            data[prefix].ylabels.push(ylabels[i]);
            data[prefix].means.push(meansUnscaled[i]);
        }

    // data[prefix].means = $.scaleTimes(data[prefix].means)   ;
    }
}
for (var pix in  prefixes){


  var xs = [];
  var p = prefixes[pix]
    console.log(p, pix);
  var prev = null;
  var benches = data[p].benches;
  var means = $.scaleTimes(data[p].means) ;
  // console.log(means);
  var meansLabel = $.timeUnits($.mean(data[p].means))[1];
  console.log("label is " + meansLabel);

   ylabels = data[p].ylabels;
   _ylabels = [];




  for (var i = 0; i < means[0].length; i++) {
    var label = benches[i].split('/')[1];
    xs.push({ label: label, data: [[means[0][i], -i]]});
    var alabel = ylabels[i][1].replace(p+'/','');
    _ylabels.push([-i, alabel] );
  }
  $("#overview").before('<h2>'+p+'</h2><div id="overview_' + pix + '" class="ovchart" style="width:900px;height:200px;"></div>');

  var oq = $("#overview_" + pix  );
  var o = $.plot(oq, xs, { bars: { show: true, horizontal: true,
      barWidth: 0.75, align: "center" },
      grid: { borderColor: "#777", hoverable: true },
      legend: { show: false/*xs.length > 1*/ },
      xaxis: { max: Math.max.apply(undefined,means[0]) * 1.02 },
      yaxis: { ticks: _ylabels, tickColor: '#ffffff' }
  });
  // console.log(ylabels);
  if (benches.length > 3){o.getPlaceholder().height(28*benches.length);}
  o.resize();
  o.setupGrid();
  o.draw();
  // that function scope trick

  (function(scaler) {
      $.addTooltip("#overview_"+pix,
          function(x,y) { return $.renderTime(x/scaler); });} )(means[1] );
};

$("#overview").remove();
});
$(document).ready(function () {
    $(".time").text(function(_, text) {
        return $.renderTime(text);
      });
    $(".citime").text(function(_, text) {
        return $.renderTime(text);
      });
    $(".percent").text(function(_, text) {
        return (text*100).toFixed(1);
      });
  });
</script>

   </div>
  </div>
  <div id="footer">
    <div class="body">
     <div class="footfirst">
      <h2>colophon</h2>
      <p>This report was created using the
	<a href="http://hackage.haskell.org/package/criterion">criterion</a>
	benchmark execution and performance analysis tool.</p>
      <p>Criterion is developed and maintained
      by <a href="http://www.serpentine.com/blog/">Bryan O'Sullivan</a>.</p>
     </div>
    </div>
  </div>
 </body>
</html>
