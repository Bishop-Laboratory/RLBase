import React from "react";
import { BrowserRouter as Router, Route } from "react-router-dom";
import { AuthProvider } from "./context/AuthContext";
import { AlertProvider } from "./context/AlertContext";
import Home from "./pages/Home";
import Alert from "./components/Alert";
import NavBar from "./components/NavBar";
import SampleView from "./pages/RLoop";
import SearchPage from "./pages/SearchPage";
import About from "./pages/About";
import ApiReference from "./pages/ApiReference";
import Downloads from "./pages/Downloads";
import Help from "./pages/Help";
import Upload from "./pages/Upload";
import Signup from "./pages/Signup";
import Login from "./pages/Login";

function App() {
  return (
    <AlertProvider>
      <AuthProvider>
        <Router>
          <Route path="/" component={NavBar} />
          <Route path="/" component={Alert} />
          <Route exact path="/" component={Home} />
          <Route exact path="/search/:type" component={SearchPage} />
          <Route exact path="/explorer" component={SampleView} />
          <Route exact path="/about" component={About} />
          <Route exact path="/api-reference" component={ApiReference} />
          <Route exact path="/downloads" component={Downloads} />
          <Route exact path="/upload" component={Upload} />
          <Route exact path="/help" component={Help} />
          <Route exact path="/signup" component={Signup} />
          <Route exact path="/login" component={Login} />
        </Router>
      </AuthProvider>
    </AlertProvider>
  );
}

export default App;
