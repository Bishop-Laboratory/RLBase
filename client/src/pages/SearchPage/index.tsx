import axios from "axios";
import React from "react";
import { RouteComponentProps } from "react-router-dom";
var qs = require("qs");
const SearchPage = ({ match, location }: RouteComponentProps) => {
  const obj = qs.parse(location.search, { ignoreQueryPrefix: true });
  console.log(location.search)
  React.useEffect(() => {
    const get = async () => {
      try {
        const res = await axios.get(
          `http://validate.jsontest.com/?json={${Object.keys(obj)[0]}:${Object.values(obj)[0]}}`
        );
        return res;
      } catch (error) {
        console.error(error);
      }
    };
    get();
  }, [obj]);
  return <p>{location.search}</p>;
};

export default SearchPage;
