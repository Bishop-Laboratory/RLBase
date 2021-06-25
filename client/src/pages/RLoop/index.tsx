import React, { useEffect, useState } from "react";
import { barChartDataItem } from "../../models";
import BarChart from "../../components/BarChart";
import { RouteComponentProps } from "react-router-dom";
import axios from "axios";

const initialState = [
  { x: "", y: 0 },
  { x: "", y: 0 },
  { x: "", y: 0 },
  { x: "", y: 0 },
  { x: "", y: 0 },
  { x: "", y: 0 },
  { x: "", y: 0 },
  { x: "", y: 0 },
];

const convertData = (row: any) => {
  const KEYS = [
    "3UTR",
    "Exon",
    "Intergenic",
    "5UTR",
    "Retroposon",
    "SINE",
    "Low_complexity",
    "Simple_repeat",
    "rRNA",
    "RNA",
    "ncRNA",
    "RC",
    "pseudo",
    "scRNA",
  ];
  let array = KEYS.map((key) => {
    return {
      x: key,
      y: Number(row[`${key}__Log2 Ratio (obs/exp)`]),
    };
  });
  return array;
};

function RLoop({ location }: RouteComponentProps) {
  const [sampleData, setSampleData] = useState<barChartDataItem[]>(
    initialState
  );
  const [minAndMax, setMinAndMax] = useState<number[]>([0, 0]);
  const [, /*loading*/ setLoading] = useState(false);
  const [info, setInfo] = useState<any>({});
  const [name, setName] = useState("");
  const [value, setValue] = useState("");

  useEffect(() => {
    const get = async () => {
      setLoading(true);
      try {
        const res: any = await axios.get(
          `http://127.0.0.1:5000/api/test/rloop-details${location.search}`
        );
        if (res?.data?.[0]) {
          setInfo(res.data[0]);
          const depuredData = convertData(res.data[0]);
          const numericData = depuredData.map((item) => item.y);
          setSampleData(depuredData);
          setMinAndMax([Math.min(...numericData), Math.max(...numericData)]);
        }
      } catch (error) {
        console.error(error);
      } finally {
        setLoading(false);
      }
    };
    get();
  }, [location.search]);
  return (
    <>
      <h3 className="mt-8 text-center">{info.SRX}</h3>
      <div>
          <ul>
            <li>Cell line/tissue: {info.cell}</li>
            <li>Sample control accession(s): {info.control}</li>
            <li>Genotype: {info.genotype}</li>
          </ul>
        </div>
      <div>
        <h3 className="mt-8 text-center">Genomic Feature Enrichment</h3>
        <div className="d-flex w-100  justify-content-center mt-2">
          <BarChart
            {...{
              title: "Gene Features",
              selectedItem: sampleData.slice(0, 4),
              minAndMax,
              color: "blue",
              setName,
              setValue,
            }}
          />
          <BarChart
            {...{
              title: "Repetitive Elements",
              selectedItem: sampleData.slice(4, 9),
              hideYAxis: true,
              minAndMax,
              color: "red",
              setName,
              setValue,
            }}
          />
          <BarChart
            {...{
              title: "RNA Species",
              selectedItem: sampleData.slice(9),
              hideYAxis: true,
              minAndMax,
              color: "green",
              setName,
              setValue,
            }}
          />
          <div
            id="tooltip"
            className="bg-dark text-light border border-info px-2 d-none position-absolute"
          >
            {name} | Log2 Ratio (obs/exp): {value}
          </div>
        </div>
      </div>
    </>
  );
}

export default RLoop;
