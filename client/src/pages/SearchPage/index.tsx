import axios from "axios";
import React from "react";
import { RouteComponentProps } from "react-router-dom";
import SearchTable from "../../components/SearchTable";

const SearchPage = ({ match, location,history }: RouteComponentProps) => {

  const [results, setResults] = React.useState<any[]>([])
  
  React.useEffect(() => {
    //@ts-ignore
    const {params:{type}} = match
    const get = async () => {
      try {
        const res: any = await axios.get(
          `http://127.0.0.1:5000/api/${type}${location.search}`
        );
        if(res) setResults(res.data)
      } catch (error) {
        console.error(error);
      }
    };
    get();
  }, [location.search, match]);
  const textStyle = "mt-2 text-center"
  return (
    <>
      {results?.length ? <p className={textStyle}> {results.length} results</p> : <p className={textStyle}>Your search did not match any documents.

</p>}
      <SearchTable
        {...{match, location, data: results?.length? results : []}}
      />
    </>
  );
};

export default SearchPage;
