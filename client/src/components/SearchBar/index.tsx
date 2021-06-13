import React, { useState } from "react";
import { useHistory } from "react-router-dom";
import { queryOption } from "../../models";

const SearchBar = ({searchPage}:{searchPage?: boolean}) => {
  const { push, replace } = useHistory();

  const queryOptions: queryOption[] = [
    { name: "Gene", query: "gene" },
    { name: "Cell type", query: "cell" },
    { name: "Region", query: "region" },
  ];

  const [searchParam, setSearchParam] = useState<string>("");
  const [queryOption, setQueryOption] = useState<string>(queryOptions[0].query);
  return (
    <form className="d-flex">
      <select
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
        value={searchParam}
        onChange={(e) => setSearchParam(e.target.value)}
        className="form-control me-2"
        type="search"
        placeholder="Find out your gene of interest"
        aria-label="Search"
      />
      <button
        className="btn btn-outline-success"
        onClick={() => searchPage ? replace({search: `${queryOption}=${searchParam}` }) : push(`/search?${queryOption}=${searchParam}`)}
        type="submit"
      >
        Search
      </button>
    </form>
  );
};

export default SearchBar;
