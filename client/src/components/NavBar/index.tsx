import React from "react";
import { Link, useHistory } from "react-router-dom";
import { useAuth } from "../../context/AuthContext";
import { useAlert } from "../../context/AlertContext";

const NavBar = () => {
  const { currentUser, logout } = useAuth();
  const { setAlert } = useAlert();

  const history = useHistory();

  async function handleLogout(e: React.FormEvent) {
    e.preventDefault();
    try {
      await logout();
      setAlert(null);
      history.push("/");
    } catch (err) {
      // TODO: error handling for failed logout
      console.log(err);
    }
  }

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
        <ul className="container navbar-nav d-flex justify-content-between">
          <span className="d-flex">
            <li className="nav-item me-1">
              <Link to="/" className="nav-link">
                RMapDB
              </Link>
            </li>
            <li className="nav-item me-1">
              <Link to="/about" className="nav-link">
                About
              </Link>
            </li>
            <li className="nav-item me-1">
              <Link to="/search/sample" className="nav-link">
                Samples
              </Link>
            </li>
            <li className="nav-item me-1">
              <Link to="/downloads" className="nav-link">
                Download
              </Link>
            </li>
            {currentUser && currentUser.emailVerified && (
              <li className="nav-item me-1">
                <Link to="/upload" className="nav-link">
                  Upload
                </Link>
              </li>
            )}
            <li className="nav-item me-1">
              <Link to="/api-reference" className="nav-link">
                API Reference
              </Link>
            </li>
            <li className="nav-item">
              <Link to="/help" className="nav-link">
                Help
              </Link>
            </li>
          </span>
          {currentUser ? (
            <li className="nav-item">
              <button
                type="button"
                className="nav-link btn btn-dark border border-light rounded"
                onClick={handleLogout}
              >
                Log Out
              </button>
            </li>
          ) : (
            <span className="d-flex">
              <li className="nav-item me-1">
                <Link to="/login" className="nav-link">
                  Log In
                </Link>
              </li>
              <li className="nav-item">
                <Link to="/signup" className="nav-link border border-light rounded">
                  Sign Up
                </Link>
              </li>
            </span>
          )}
        </ul>
      </div>
    </nav>
  );
};

export default NavBar;
