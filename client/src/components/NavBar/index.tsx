import React from "react";
import { Link } from "react-router-dom";

const NavBar = (): any => {
  return (
    <nav className="navbar navbar-expand-lg navbar-dark bg-dark">
      <button
        className="navbar-toggler"
        type="button"
        data-toggle="collapse"
        data-target="#navbarNav"
        aria-controls="navbarNav"
        aria-expanded="false"
        aria-label="Toggle navigation"
      >
        <span className="navbar-toggler-icon" />
      </button>
      <div className="collapse navbar-collapse" id="navbarNav">
        <ul className="navbar-nav">
          <li className="nav-item">
            <Link to="/" className="nav-link">
              RMapDB
            </Link>
          </li>
          <li className="nav-item">
            <Link to="/about" className="nav-link">
              About
            </Link>
          </li>
          <li className="nav-item">
            <Link to="/search/gene" className="nav-link">
              Genes
            </Link>
          </li>
          <li className="nav-item">
            <Link to="/search/r-loop" className="nav-link">
              RLoops
            </Link>
          </li>
          <li className="nav-item">
            <Link to="/search/sample" className="nav-link">
              Samples
            </Link>
          </li>
          <li className="nav-item">
            <Link to="/downloads" className="nav-link">
              Download
            </Link>
          </li>
          <li className="nav-item">
            <Link to="/upload" className="nav-link">
              Upload
            </Link>
          </li>
          <li className="nav-item">
            <Link to="/api-reference" className="nav-link">
              API Reference
            </Link>
          </li>
          <li className="nav-item">
            <Link to="/help" className="nav-link">
              Help
            </Link>
          </li>
        </ul>
      </div>
    </nav>
  );
};

export default NavBar;
