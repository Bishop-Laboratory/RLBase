import axios from "axios";
import React from "react";
import { RouteComponentProps } from "react-router-dom";
import SearchTable from "../../components/SearchTable";
var qs = require("qs");

const SearchPage = ({ match, location }: RouteComponentProps) => {
  const obj = qs.parse(location.search, { ignoreQueryPrefix: true });
  console.log(location.search);
  const data = [
    {
      rloop: "SRX1233333", // accessor is the "key" in the data
      type: "unknown",
      info: "sample",
      evidence: "null",
    },
  ]
  React.useEffect(() => {
    const get = async () => {
      try {
        const res = await axios.get(
          `http://validate.jsontest.com/?json={${Object.keys(obj)[0]}:${
            Object.values(obj)[0]
          }}`
        );
        return res;
      } catch (error) {
        console.error(error);
      }
    };
    get();
  }, [obj]);
  return (
    <>
      <p>{location.search}</p>
      {data?.length ? <p>About {data.length} results</p> : <p>Your search - {Object.values(obj)[0]} - did not match any documents.

</p>}
      <SearchTable
        {...{data}}
      />
    </>
  );
};

export default SearchPage;
