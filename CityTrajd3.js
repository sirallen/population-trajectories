(function() {

Shiny.addCustomMessageHandler('jsondata', function(message) {
  cities = message[0].map(parse_el);

  if (d3.select('#d3io').select('svg').empty()) {
    makeChart(cities)
  };
  updateAxes(cities);
  updateChart(cities);

});

Shiny.addCustomMessageHandler('CurveFitMethod', function(message) {
  method = message[0];
  if (method=='d3.curveBasis') {
    curveFun = d3.curveBasis;
  } else if (method=='None') {
    curveFun = d3.curveLinear;
  } else {
    return null
  }
  line.curve(curveFun);
  updateChart(cities);

});

/* Top-level variables */
var svg, plotWidth, plotHeight, scaleX, scaleY, cities, baseYear = 2000;

var line, curveFun = d3.curveBasis;

parse_el = function(el) {
    var rowdata = [];
    for (var i=0; i < el.YEAR.length; i++) {
      if (el.YEAR[i] >= baseYear) {
        rowdata.push(
          {CITY: el.CITY, YEAR: el.YEAR[i], POP_Idx: el.POP_Idx[i]}
        )
      };

    };
    return rowdata;
  };


makeChart = function(cities) {

  var margin = {top: 20, right: 100, bottom: 30, left: 50},
      svgWidth = 800,
      svgHeight = 800;

  /* Define top-level variables */
  plotWidth = svgWidth - margin.left - margin.right;
  plotHeight = svgHeight - margin.top - margin.bottom;

  svg = d3.select('#d3io').append('svg')
          .attr('width', svgWidth)
          .attr('height', svgHeight)
          .style('border', '1px lightgray solid')
        .append('g')
          .attr('transform',
            'translate(' + margin.left + ',' + margin.top + ')');

  svg.append('g').attr('class', 'grid grid--x')
  svg.append('g').attr('class', 'grid grid--y')
  svg.append('g').attr('class', 'axis axis--x')
  svg.append('g').attr('class', 'axis axis--y')

};

updateAxes = function(cities) {
  gen_x_ticks = function(xrange) {
    /* Generate a sequence of x ticks */
    var interval = (xrange[1] - xrange[0] > 10) ? 2 : 1;
    var a = Math.ceil(xrange[0]);
    var b = Math.floor(xrange[1]);
    var xtickSeq = [];
    for (var i=a; i <= b; i += interval) {
      xtickSeq.push(i)
    };
    return xtickSeq
  };

  gen_y_ticks = function(yrange, interval=5) {
    /* Generate a sequence of y ticks */
    var a = interval * Math.ceil(yrange[0] / interval);
    var b = interval * Math.floor(yrange[1] / interval);
    var ytickSeq = [];
    for (var i=a; i <= b; i += interval) {
      ytickSeq.push(i)
    };
    return ytickSeq
  };

  expand_range = function(range, k) {
    /* Expand a range [a, b] by factor k about its midpoint */
    var mid = (range[1] + range[0]) / 2.0;
    return range.map(function(z) { return (z - mid) * k + mid })
  };

  /* Define/Update top-level variables */
  scaleX = d3.scaleLinear()
        .domain(expand_range(
          d3.extent(cities[0].map(function(el) {
            return el.YEAR})
          ), 1.05))
        .range([0, plotWidth]),
  scaleY = d3.scaleLinear()
        .domain(
          expand_range([
          d3.min(cities.map(function(city) {
            return d3.min(city.map(function(el) {
              return el.POP_Idx}))})),
          d3.max(cities.map(function(city) {
            return d3.max(city.map(function(el) {
              return el.POP_Idx}))}))
          ], 1.05))
        .range([plotHeight, 0]),
  line = d3.line()
           .curve(curveFun)
           .x(function(d) { return scaleX(d.YEAR) })
           .y(function(d) { return scaleY(d.POP_Idx)});

  svg.select('g.grid--x')
     .attr('transform', 'translate(0,' + plotHeight + ')')
     .call(d3.axisBottom(scaleX).tickFormat('').tickSize(-plotHeight)
             .tickValues(gen_x_ticks(scaleX.domain())));

  svg.select('g.grid--y')
     .call(d3.axisLeft(scaleY).tickFormat('').tickSize(-plotWidth)
             .tickValues(gen_y_ticks(scaleY.domain())));

  svg.select('g.axis--x')
     .attr('transform', 'translate(0,' + plotHeight + ')')
     .call(d3.axisBottom(scaleX).tickValues(gen_x_ticks(scaleX.domain()))
             .tickFormat(d3.format('d')));

  svg.select('g.axis--y')
     .call(d3.axisLeft(scaleY).tickValues(gen_y_ticks(scaleY.domain())));

};

updateChart = function(cities) {

  svg.selectAll('g.city')
     .data([])
     .exit()
     .remove()

  var city =
    svg.selectAll('g.city')
       .data(cities)
       .enter()
       .append('g')
       .attr('class', 'city')
       .on('mouseover', function(z) {
        d3.select(this).select('.line').transition()
          .duration(250)
          .style('stroke-width', '3px')
          .style('stroke', 'black')
        d3.select(this).select('.label').transition()
          .duration(250)
          .style('fill', 'black')

      })
      .on('mouseout', function(z) {
        d3.select(this).select('.line').transition()
          .duration(250)
          .style('stroke-width', '1.5px')
          .style('stroke', 'gray')
        d3.select(this).select('.label').transition()
          .duration(250)
          .style('fill', 'gray')
      });;

  city.append('path')
      .attr('class', 'line')
      .attr('d', line);

  city.append('path')
      .attr('class', 'transparent')
      .attr('d', line);

  city.append('text')
      .datum(function(d) {
        return {
          name: d[0].CITY,
          xpos: d[d.length - 1].YEAR,
          ypos: d[d.length - 1].POP_Idx
        }
      })
      .attr('transform', function(d) {
        return 'translate(' + scaleX(d.xpos) + ',' + scaleY(d.ypos) + ')' })
      .attr('class', 'label')
      .attr('dx', 5)
      .attr('dy', '0.35em')
      .text(function(d) { return(d.name) });
};


})();