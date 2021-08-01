import React from "react";
import { BrowserRouter as Router, Route } from "react-router-dom";
import Home from "./pages/Home";
import NavBar from "./components/NavBar";
import SampleView from "./pages/RLoop";
import SearchPage from "./pages/SearchPage";
import About from "./pages/About";
import ApiReference from "./pages/ApiReference";
import Downloads from "./pages/Downloads";
import Help from "./pages/Help";
import Upload from "./pages/Upload";

function App() {

  return (
    <Router>
      <Route path="/" component={NavBar} />
      <Route exact path="/" component={Home} />
      <Route exact path="/search/:type" component={SearchPage} />
      <Route exact path="/explorer" component={SampleView} />
      <Route exact path="/about" component={About} />
      <Route exact path="/api-reference" component={ApiReference} />
      <Route exact path="/downloads" component={Downloads} />
      <Route exact path="/upload" component={Upload} />
      <Route exact path="/help" component={Help} />
    </Router>
  );
}


export default App;
