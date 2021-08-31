import React, { useState } from "react";
import { Link, useHistory } from "react-router-dom";
import { useAuth } from "../../context/AuthContext";
import SigninButton from "../../components/SigninButton";
import GoogleLogo from "../../assets/GoogleLogo.png";
import GithubLogo from "../../assets/GithubLogo.png";

export default function Login(): JSX.Element {
  const { loginWithEmailAndPAssword, signInWithGoogle, signInWithGithub } = useAuth();
  const history = useHistory();

  const [email, setEmail] = useState<string>("");
  const [password, setPassword] = useState<string>("");
  const [loading, setLoading] = useState<boolean>(false);
  const [error, setError] = useState<string>("");

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    try {
      setError("");
      setLoading(true);
      await loginWithEmailAndPAssword(email, password);
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
        <h1 className="h2 text-center">Log In</h1>
        {error && (
          <div className="alert alert-danger mt-3" role="alert">
            {error}
          </div>
        )}
        <SigninButton
          provider="Google"
          logo={GoogleLogo}
          handleSignIn={signInWithGoogle}
          setError={setError}
        />
        <SigninButton
          provider="Github"
          logo={GithubLogo}
          handleSignIn={signInWithGithub}
          setError={setError}
        />
        <div className="mb-3">
          <label htmlFor="loginEmail" className="form-label">
            Email
          </label>
          <input
            type="email"
            className="form-control"
            id="loginEmail"
            aria-describedby="email"
            value={email}
            onChange={(e) => setEmail(e.target.value)}
            autoComplete="username"
            required
          />
        </div>
        <div className="mb-3">
          <label htmlFor="loginPassword" className="form-label">
            Password
          </label>
          <input
            type="password"
            className="form-control"
            id="loginPassword"
            aria-describedby="password"
            value={password}
            onChange={(e) => setPassword(e.target.value)}
            autoComplete="current-password"
            required
          />
        </div>
        <button type="submit" className="btn btn-primary" disabled={loading}>
          Log in
        </button>
        <p className="mt-3 text-center">
          Need an account? <Link to="/signup">Sign up</Link>
        </p>
      </form>
    </main>
  );
}
