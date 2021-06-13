import React, { useEffect, useState } from "react";
import { barChartDataItem } from "../../models";
import BarChart from "../../components/BarChart";
import { RouteComponentProps } from "react-router-dom";
import axios from "axios";

const initialState = [{x:"",y:0},{x:"", y: 0},{x:"",y:0},{x:"", y: 0},{x:"",y:0},{x:"", y: 0},{x:"",y:0},{x:"", y: 0},]

const convertData = (row: any) => {
  const KEYS = [
    "3utr",
    "tts",
    "exon",
    "intron",
    "intergenic",
    "5utr",
  ];
  let array = KEYS.map((key) => {
    return {
      x: key,
      y: Number(row[`log2_ratio_${key}`]),
    };
  });
  return array;
};


function RLoop({location}:RouteComponentProps) {
  
  const [sampleData, setSampleData] =  useState<barChartDataItem[]>(initialState)
  const [, /*loading*/ setLoading] = useState(false);
  const [info, setInfo] = useState<any>({})
  useEffect(() => {
    const get = async () => {
      setLoading(true)
      try {
        const res: any = await axios.get(
          `http://127.0.0.1:5000/api/test/rloop-details${location.search}`
        );
        if(res?.data?.[0]) {
          setInfo(res.data[0])
          setSampleData(convertData(res.data[0]))}
      } catch (error) {
        console.error(error);
      } finally {
        setLoading(false)
      }
    };
    get();
  }, [location.search]);
  return (
    <>
    <h3 className="mt-2 text-center">{info.srx}</h3>
    <div className="d-flex mt-2">
      <BarChart {...{selectedItem: sampleData}} />
      <div>
        <ul>
          <li>Cell line/tissue: {info.cell}</li>
          <li>Sample control accession(s): {info.control}</li>
          <li>Genotype: {info.genotype}</li>
        </ul>
      </div>
    </div>
    </>
  );
}

export default RLoop;