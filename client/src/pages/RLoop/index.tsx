import React, { useEffect, useState } from "react";
import {
  csv,
} from "d3";
import { barChartDataItem } from "../../models";
import Table from "../../components/Table";
import BarChart from "../../components/BarChart";

const initialState = [{x:"",y:0},{x:"", y: 0},{x:"",y:0},{x:"", y: 0},{x:"",y:0},{x:"", y: 0},{x:"",y:0},{x:"", y: 0},]

function RLoop() {
  const [data, setData] = useState<any[]>([]);
  const [, /*loading*/ setLoading] = useState(false);
  const [selectedItem, setSelectedItem] = useState<barChartDataItem[]>(initialState);
  useEffect(() => {
    csv("/data/rmap_full_11_25_with_study.csv").then((d) => {
      setData(d);
      setLoading(false);
    });
    return () => undefined;
  }, []);

  return (
    <div className="d-flex mt-2">
      <BarChart {...{selectedItem}} />
      <Table {...{data, setSelectedItem}} />
    </div>
  );
}

export default RLoop;