import React, { useEffect, useState } from "react";
import { RouteComponentProps } from "react-router-dom";
import axios from "axios";
import { barChartDataItem } from "../../models";
import BarChart from "../../components/BarChart";
import SampleDownloads from "../../components/SampleDownloads";

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
    "CpG-Island",
    "Promoter",
    "5UTR",
    "Exon",
    "Intron",
    "3UTR",
    "TTS",
    "Intergenic",
    "miRNA",
    "ncRNA",
    "pseudo",
    "RC",
    "RNA",
    "scRNA",
    "snoRNA",
    "snRNA",
    "srpRNA",
    "tRNA",
    "DNA",
    "LINE",
    "Low_complexity",
    "LTR",
    "Retroposon",
    "rRNA",
    "Satellite",
    "Simple_repeat",
    "SINE",
  ];
  const array = KEYS.map((key) => {
    return {
      x: key,
      y: Number(row[`${key}__Log2 Ratio (obs/exp)`]),
    };
  });
  return array;
};

function SampleView({ location }: RouteComponentProps) {
  const [sampleData, setSampleData] = useState<barChartDataItem[]>(initialState);
  const [minAndMax, setMinAndMax] = useState<number[]>([0, 0]);
  const [, /* loading */ setLoading] = useState(false);
  const [info, setInfo] = useState<any>({});
  const [name, setName] = useState("");
  const [value, setValue] = useState("");

  useEffect(() => {
    const get = async () => {
      setLoading(true);
      try {
        const res: any = await axios.get(`http://127.0.0.1:5000/api-v1/sample${location.search}`);
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
      <h1 className="mt-8 text-center">RSeq Report</h1>
      <h3 className="mt-8 text-center">{info.SRX}</h3>
      <div>
        <ul>
          <li>Cell line/tissue: {info.cell}</li>
          <li>Sample control accession(s): {info.control}</li>
          <li>Genotype: {info.genotype}</li>
        </ul>
      </div>
      <div className="ms-4">
        <h3 className="mt-8 text-center">Genomic Feature Enrichment</h3>
        <p>
          R-loop broad peaks were called with <code>macs2</code> and then compared with genomic
          features using <code>assignGenomeAnnotation</code> from <code>homer</code>.
        </p>
        <div className="d-flex w-100  justify-content-center mt-2">
          <BarChart
            {...{
              title: "Genomic Features",
              selectedItem: sampleData.slice(0, 8),
              minAndMax,
              color: "blue",
              setName,
              setValue,
            }}
          />
          <BarChart
            {...{
              title: "ncRNAs",
              selectedItem: sampleData.slice(8, 18),
              hideYAxis: true,
              minAndMax,
              color: "red",
              setName,
              setValue,
            }}
          />
          <BarChart
            {...{
              title: "Repetitive elements",
              selectedItem: sampleData.slice(18),
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

        <SampleDownloads sampleName={info.sample_name} sampleGenome={info.genome} />
      </div>
    </>
  );
}

export default SampleView;
