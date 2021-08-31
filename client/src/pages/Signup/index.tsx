import React, { useState } from "react";
import { Link, useHistory } from "react-router-dom";
import firebase from "firebase";
import { useAuth } from "../../context/AuthContext";

export default function Signup(): JSX.Element {
  const { signup, sendEmailVerification } = useAuth();
  const history = useHistory();

  const [email, setEmail] = useState<string>("");
  const [password, setPassword] = useState<string>("");
  const [passwordConfirm, setPasswordConfirm] = useState<string>("");
  const [loading, setLoading] = useState<boolean>(false);
  const [error, setError] = useState<string>("");

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    if (password !== passwordConfirm) {
      return setError("Passwords do not match");
    }
    try {
      setError("");
      setLoading(true);
      const userData: firebase.auth.UserCredential = await signup(email, password);
      if (userData.user) {
        await sendEmailVerification(userData.user);
      }
      history.push("/");
    } catch (err) {
      setError(err.message);
    } finally {
      setLoading(false);
    }
  };

  return (
    <main className="container">
      <form className="auth-form card p-4 mt-5" onSubmit={handleSubmit}>
        <h1 className="h2 text-center">Sign Up</h1>
        {error && (
          <div className="alert alert-danger mt-3" role="alert">
            {error}
          </div>
        )}
        <div className="mb-3">
          <label htmlFor="signupEmail" className="form-label">
            Email
          </label>
          <input
            type="email"
            className="form-control"
            id="signupEmail"
            aria-describedby="email"
            value={email}
            onChange={(e) => setEmail(e.target.value)}
            required
          />
        </div>
        <div className="mb-3">
          <label htmlFor="signupPassword" className="form-label">
            Password
          </label>
          <input
            type="password"
            className="form-control"
            id="signupPassword"
            aria-describedby="password"
            value={password}
            onChange={(e) => setPassword(e.target.value)}
            required
          />
          <div className="form-text">Must be at least 6 characters long</div>
        </div>
        <div className="mb-3">
          <label htmlFor="signupPasswordConfirmation" className="form-label">
            Confirm Password
          </label>
          <input
            type="password"
            className="form-control"
            id="signupPasswordConfirmation"
            aria-describedby="passwordConfirmation"
            value={passwordConfirm}
            onChange={(e) => setPasswordConfirm(e.target.value)}
            required
          />
        </div>
        <button type="submit" className="btn btn-primary" disabled={loading}>
          Sign up
        </button>
        <p className="mt-3 text-center">
          Already have an account? <Link to="/login">Log in</Link>
        </p>
      </form>
    </main>
  );
}
