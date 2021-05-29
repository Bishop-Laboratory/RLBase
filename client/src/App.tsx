import React from "react";
import { BrowserRouter as Router, Route } from "react-router-dom";
import Home from "./components/Home";
import NavBar from "./components/NavBar";



function App() {

  return (
    <Router>
      <Route path="/" component={NavBar} />
      <Route exact path="/" component={Home} />
    </Router>
  );
}


export default App;
