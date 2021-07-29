import React from "react";

type DownloadButtonVariant = "bigwig" | "peaks" | "report";

interface DownloadButtonInfo {
  path: string;
  label: string;
  fileName: string;
}

const getFileInfo = (
  sampleName: string,
  sampleGenome: string,
  variant: DownloadButtonVariant
): DownloadButtonInfo => {
  switch (variant) {
    case "bigwig":
      return {
        path: "bigwigs/rseq-coverage-unstranded",
        label: "BigWig",
        fileName: `${sampleName}.${sampleGenome}.bw`,
      };
    case "peaks":
      return {
        path: "final-peaks-unstranded",
        label: "Peaks",
        fileName: `${sampleName}_${sampleGenome}.unstranded.bed`,
      };
    case "report":
      return {
        path: "rseq-quality-reports",
        label: "Report",
        fileName: `${sampleName}_${sampleGenome}.QC_report.html`,
      };
  }
};

function DownloadButton({
  sampleName,
  sampleGenome,
  variant,
}: {
  sampleName: string;
  sampleGenome: string;
  variant: DownloadButtonVariant;
}) {
  const fileInfo = getFileInfo(sampleName, sampleGenome, variant);
  let url = `https://rmapdb-data.s3.us-east-2.amazonaws.com/${fileInfo.path}/${fileInfo.fileName}`;
  return (
    <a
      className="btn btn-primary mx-1"
      href={url}
      target="_blank"
      rel="noopener noreferrer"
    >
      {fileInfo.label}
    </a>
  );
}

export default DownloadButton;
