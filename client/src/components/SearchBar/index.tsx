import React, { useState } from "react";
import { useHistory } from "react-router-dom";
import { queryOption } from "../../models";

const SearchBar = ({searchPage}:{searchPage?: boolean}) => {
  const { push } = useHistory();

  const queryOptions: queryOption[] = [
   // { name: "Gene", query: "gene" },
    { name: "Cell type", query: "Cell" },
   // { name: "Region", query: "region" },
  ];
  const [radio, setRadio] = useState<"gene"|"r-loop"|"sample">("gene")
  const [searchParam, setSearchParam] = useState<string>("");
  const [queryOption, setQueryOption] = useState<string>(queryOptions[0].query);
  return (
    <>
    <h5>What do you want to look for?</h5>
    <div className="btn-group" role="group" aria-label="Basic radio toggle button group">
  <input type="radio" className="btn-check" name="btnradio" id="btnradio1" autoComplete="off" onClick={() => setRadio("gene")} checked={radio === "gene"} />
  <label className="btn btn-outline-primary" htmlFor="btnradio1">Gene</label>

  <input type="radio" className="btn-check" name="btnradio" id="btnradio2" autoComplete="off" onClick={() => setRadio("r-loop")} checked={radio === "r-loop"} />
  <label className="btn btn-outline-primary" htmlFor="btnradio2">R-Loop</label>

  <input type="radio" className="btn-check" name="btnradio" id="btnradio3" autoComplete="off" onClick={() => setRadio("sample")} checked={radio === "sample"} />
  <label className="btn btn-outline-primary" htmlFor="btnradio3">Sample</label>
</div>
    <form className="mt-4 d-flex">
      <select
        className="me-2"
        value={queryOption}
        onChange={(e) => setQueryOption(e.target.value)}
      >
        {queryOptions.map((option, index) => (
          <option key={"query" + index} value={option.query}>
            {option.name}
          </option>
        ))}
      </select>
      <input
        style={{width: 300}}
        value={searchParam}
        onChange={(e) => setSearchParam(e.target.value)}
        className="form-control me-2"
        type="search"
        placeholder={`Find out your ${radio} of interest`}
        aria-label="Search"
      />
      <button
        className="btn btn-outline-success"
        onClick={() => push(`/search/${radio}?${queryOption}=${searchParam}`)}
        type="submit"
      >
        Search
      </button>
    </form>
    </>
  );
};

export default SearchBar;
