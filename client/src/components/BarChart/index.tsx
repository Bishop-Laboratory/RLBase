import React, { useRef, useEffect, useState } from "react";
import {
  select,
  axisBottom,
  scaleLinear,
  scaleBand,
  min,
  max,
  axisLeft,
  pointer,
} from "d3";
import { barChartDataItem } from "../../models";

function BarChart({ selectedItem }: { selectedItem: barChartDataItem[] }) {
  const [name, setName] = useState("")
  const [value, setValue] = useState("")
  const svgRef = useRef<any>();

  useEffect(() => {
    const MARGIN = { LEFT: 45, RIGHT: 30, TOP: 100, BOTTOM: 100 };
    const HEIGHT = 500 - MARGIN.TOP - MARGIN.BOTTOM;
    const WIDTH = 300 - MARGIN.LEFT - MARGIN.RIGHT;
    const svg: any = select(svgRef.current);

    svg
      .attr("height", HEIGHT + MARGIN.TOP + MARGIN.BOTTOM)
      .attr("width", WIDTH + MARGIN.LEFT + MARGIN.RIGHT);

    const xScale = scaleBand()
      .domain(selectedItem.map((value: barChartDataItem) => value.x))
      .range([0, WIDTH])
      .padding(0.2);

    const xAxis: any = axisBottom(xScale).ticks(selectedItem.length);
    svg
      .select(".x-axis")
      .style(
        "transform",
        `translate(${MARGIN.LEFT}px,${HEIGHT + MARGIN.TOP}px)`
      )
      .call(xAxis)
      .selectAll("text")
      .attr("y", "-5")
      .attr("x", "-5")
      .attr("text-anchor", "end")
      .attr("transform", "rotate(-90)");

    svg.select(".x-axis").selectAll(".tick").selectAll("line").remove();
    const yScale = scaleLinear()
      .domain([
        min(selectedItem.map((number) => number.y)) as number,
        max(selectedItem.map((number) =>  number.y > 0 ? number.y : 0)) as number,
      ])
      .range([HEIGHT, 0]);

    const yAxis = axisLeft(yScale);
    svg
      .select(".y-axis")
      .transition()
      .style("transform", `translate(${MARGIN.LEFT}px,${MARGIN.TOP}px)`)
      .call(yAxis)
      .selectAll("text")
      .attr("text-anchor", "end")
    svg
        .append("text")
        .text("Log2 Ratio (obs/exp)")
        .attr("transform", `translate(15, ${HEIGHT - MARGIN.TOP/2}) rotate(-90)`)
        .attr("text-anchor", "middle")
    svg
      .selectAll(".bar")
      .data(selectedItem)
      .on("mouseover", (e: any) => {
        setName(e.target.getAttribute("name"))
        setValue(e.target.getAttribute("value"))
        select("#tooltip")
          .classed("d-none", false)
          .style("left", pointer(e)[0] + "px")
          .style("top", pointer(e)[1] + "px");
      })
      .on("mouseout", (e:any) => {
        select('#tooltip')
        .classed("d-none", true);
      })
      .join("rect")
      .transition()
      .attr("class", "bar")
      .attr("x", (value: barChartDataItem) => {
        //@ts-ignore
        return xScale(value.x) + MARGIN.LEFT;
      })
      .attr("y", (value: barChartDataItem) =>
        value.y < 0 ? MARGIN.TOP + yScale(0) : MARGIN.TOP + yScale(value.y)
      )
      .attr("width", xScale.bandwidth)
      .attr("height", (value: barChartDataItem) =>
        Math.abs(yScale(0) - yScale(value.y))
      )
      .attr("value", (value: barChartDataItem) => value.y)
      .attr("name", (value: barChartDataItem) => value.x)
      .attr("fill", "green");
  }, [selectedItem]);

  return (
    <div className="ms-5">
      <h4 className="text-center">Gene Features</h4>
      <div id="tooltip" className="bg-dark text-light border border-info px-2 d-none position-absolute">
        {name} | Log2 Ratio (obs/exp): {value}
      </div>
      <svg ref={svgRef}>
        <g className="x-axis" />
        <g className="y-axis" />
      </svg>
    </div>
  );
}

export default BarChart;
