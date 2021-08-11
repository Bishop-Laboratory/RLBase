import React from "react";
import SampleDownloadButton from "../SampleDownloadButton";

function SampleDownloads({
  sampleName,
  sampleGenome,
}: {
  sampleName: string;
  sampleGenome: string;
}) {
  return (
    <section className="mb-3">
      <h3 className="text-center">Get RSeq Data</h3>
      <div className="w-100 d-flex justify-content-center mt-3">
        <SampleDownloadButton
          sampleName={sampleName}
          sampleGenome={sampleGenome}
          variant="bigwig"
        />
        <SampleDownloadButton sampleName={sampleName} sampleGenome={sampleGenome} variant="peaks" />
        <SampleDownloadButton
          sampleName={sampleName}
          sampleGenome={sampleGenome}
          variant="report"
        />
      </div>
    </section>
  );
}

export default SampleDownloads;
