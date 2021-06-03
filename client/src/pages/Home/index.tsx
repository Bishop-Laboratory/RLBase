import React, { useState } from "react";
import {useHistory} from "react-router-dom"; 
  
const Home = () => {
  const {push} = useHistory()
  const [search, setSearch] = useState<string>("")
return (
  <div className="mt-4 w-100 d-flex flex-column align-items-center">
    <form className="d-flex w-50">
      <input value={search} onChange={(e) => setSearch(e.target.value)} className="form-control me-2" type="search" 
      placeholder="Find out your gene of interest" aria-label="Search" />
      <button className="btn btn-outline-success" onClick={() => push(`/search?key=${search}`)} type="submit">Search</button>
    </form>
    <div className="bg-light p-5 rounded-lg m-3">
  <h5 className="">About R-loop</h5>
  <p>An R-loop is a three-stranded nucleic acid structure, formed during transcription,
  which comprises of nascent RNA hybridized with the DNA template, leaving the non-template DNA single-stranded.</p>
<p>
R-loop is often formed during transcription and its formation has been observed for several species. 
Their roles in the regulation of transcription, splicing, telomere maintenance, genome instability, 
mutagenesis, cell proliferation and differentiation as well as diseases involvement have been demonstrated.</p>
</div>
  </div>
  );
}

export default Home;