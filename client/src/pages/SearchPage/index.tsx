/* eslint-disable @typescript-eslint/ban-ts-comment */
/* eslint-disable no-nested-ternary */
import axios from "axios";
import React from "react";
import { RouteComponentProps } from "react-router-dom";
import SearchTable from "../../components/SearchTable";

const SearchPage = ({ match, location }: RouteComponentProps) => {
  const [results, setResults] = React.useState<any[]>([]);
  const [loading, setLoading] = React.useState<boolean>(false);
  React.useEffect(() => {
    const {
      // @ts-ignore
      params: { type },
    } = match;
    const get = async () => {
      setLoading(true);
      try {
        const res: any = await axios.get(`http://127.0.0.1:5000/api-v1/${type}${location.search}`);
        if (res) setResults(res.data);
      } catch (error) {
        console.error(error);
      } finally {
        setLoading(false);
      }
    };
    get();
  }, [location.search, match]);
  const textStyle = "mt-2 text-center";
  return (
    <>
      {loading ? (
        <div style={{ marginTop: 50 }} className="w-100 d-flex justify-content-center">
          <div className="spinner-border text-info" role="status">
            <span className="sr-only" />
          </div>
        </div>
      ) : null}
      {!loading ? (
        results?.length ? (
          <p className={textStyle}> {results.length} results</p>
        ) : (
          <p className={textStyle}>Your search did not match any documents.</p>
        )
      ) : null}
      {!loading ? (
        <SearchTable {...{ match, location, data: results?.length ? results : [] }} />
      ) : null}
    </>
  );
};

export default SearchPage;
