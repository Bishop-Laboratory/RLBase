import React, { useState, useEffect } from "react";
import { useHistory } from "react-router-dom";

const SearchBar = () => {
  const { push } = useHistory();

  const [radio, setRadio] = useState<"gene" | "r-loop" | "sample">("sample");
  const [parameter,setParameter] = useState<string>("")
  const [currentOptions, setCurrentOptions] = useState<any>({})
  const [inputValue, setInputValue] = useState<string>("")

  const navigateToSearchResults = () => {

    push(`/search/${radio}?${parameter}=${inputValue}`)
  }
  useEffect(()=> {

    const sampleOptions = {"Cell": "Cell type", study: "Study", Species: "Specie"}
    const geneOptions = {"Gene ID": "Gene", go: "GO Biological process"}
    const rloopOptions = {"R-loop": "R-loop ID", coordinates: "Genomic coordinates", class: "R-loop class"}

    if(radio === "gene") {
    setCurrentOptions(geneOptions)}
    if(radio === "r-loop"){ 
    setCurrentOptions(rloopOptions)}
    if(radio === "sample") {
    setCurrentOptions(sampleOptions)}

  },[radio])
  return (
    <>
      <h5>What do you want to look for?</h5>
      <div
        className="btn-group"
        role="group"
        aria-label="Basic radio toggle button group"
      >
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
        <div className="d-flex flex-row">
        <select onChange={e => setParameter(e.target.value)} style={{width: 300}}>
          {Object.keys(currentOptions).map((item, index) => <option value={item}>{currentOptions[item]}</option>)}
        </select>
        <input
              value={inputValue}
              onChange={(e)=> setInputValue(e.target.value)}
              type="text"
              className="form-control"
            /></div>
        <button
          style={{width: 300}}
          className="mt-4 align-self-center btn btn-outline-success"
          onClick={navigateToSearchResults}
          type="submit"
        >
          Search
        </button>
      </form>
    </>
  );
};

export default SearchBar;
