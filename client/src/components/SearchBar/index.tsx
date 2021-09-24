/* eslint-disable react/no-array-index-key */
import { stringify } from "qs";
import React, { useState, useEffect } from "react";
import { useHistory } from "react-router-dom";

const SearchBar = () => {
  const { push } = useHistory();
  const sampleOptions = {
    Cell: "Cell type",
    study: "Study",
    Species: "Specie",
  };
  const geneOptions = { "Gene ID": "Gene", go: "GO Biological process" };
  const rloopOptions = {
    "R-loop": "R-loop ID",
    coordinates: "Genomic coordinates",
    class: "R-loop class",
  };
  const [radio, setRadio] = useState<"gene" | "r-loop" | "sample">("sample");
  const [parameters, setParameters] = useState<any>({});
  const [currentOptions, setCurrentOptions] = useState<any>({});
  const navigateToSearchResults = () => {
    const filters = { ...parameters };
    Object.keys(filters).forEach((parameter) => {
      if (filters[parameter] === "") delete filters[parameter];
    });
    push(`/search/${radio}?${stringify(filters)}`);
  };
  useEffect(() => {
    const renderEmptyInput = (object: any) => {
      const cleanState = { ...object };
      Object.keys(cleanState).forEach((parameter) => {
        cleanState[parameter] = "";
      });
      return cleanState;
    };
    if (radio === "gene") {
      setParameters(() => ({ ...renderEmptyInput(geneOptions) }));
      setCurrentOptions(geneOptions);
    }
    if (radio === "r-loop") {
      setParameters(() => ({ ...renderEmptyInput(rloopOptions) }));
      setCurrentOptions(rloopOptions);
    }
    if (radio === "sample") {
      setParameters(() => ({ ...renderEmptyInput(sampleOptions) }));
      setCurrentOptions(sampleOptions);
    }
  }, [radio]);
  return (
    <>
      <h5>What do you want to look for?</h5>
      <div className="btn-group" role="group" aria-label="Basic radio toggle button group">
        <input
          type="radio"
          className="btn-check"
          name="btnradio"
          id="btnradio1"
          autoComplete="off"
          onClick={() => setRadio("gene")}
          checked={radio === "gene"}
        />
        <label className="btn btn-outline-primary" htmlFor="btnradio1">
          Gene
        </label>

        <input
          type="radio"
          className="btn-check"
          name="btnradio"
          id="btnradio2"
          autoComplete="off"
          onClick={() => setRadio("r-loop")}
          checked={radio === "r-loop"}
        />
        <label className="btn btn-outline-primary" htmlFor="btnradio2">
          R-Loop
        </label>

        <input
          type="radio"
          className="btn-check"
          name="btnradio"
          id="btnradio3"
          autoComplete="off"
          onClick={() => setRadio("sample")}
          checked={radio === "sample"}
        />
        <label className="btn btn-outline-primary" htmlFor="btnradio3">
          Sample
        </label>
      </div>
      <h5 className="mt-3">Find {radio}s with...</h5>
      <form className="d-flex flex-column">
        {Object.keys(currentOptions).map((item, index) => (
          <div key={`input-${index}`} className="input-group mb-3">
            <span className="input-group-text" id={`inputGroup-${index}`}>
              {/* @ts-ignore */}
              {currentOptions[item]}
            </span>
            <input
              value={parameters[item]}
              onChange={(e) =>
                setParameters((prev: any) => ({
                  ...prev,
                  [item]: e.target.value,
                }))
              }
              type="text"
              className="form-control"
              aria-label={`Search by ${item}`}
            />
          </div>
        ))}
        <button className="btn btn-outline-success" onClick={navigateToSearchResults} type="submit">
          Search
        </button>
      </form>
    </>
  );
};

export default SearchBar;
