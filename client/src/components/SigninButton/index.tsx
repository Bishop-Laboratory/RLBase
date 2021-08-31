import React from "react";
import { useHistory } from "react-router-dom";
import firebase from "firebase";
import styles from "./index.module.css";

export default function SignInButton({
  provider,
  logo,
  handleSignIn,
  setError,
}: {
  provider: string;
  logo: string;
  handleSignIn: () => Promise<firebase.auth.UserCredential>;
  setError: (error: string) => void;
}): JSX.Element {
  const history = useHistory();

  const handleSigninWithProvider = async (
    e: React.FormEvent,
    signInFunction: () => Promise<firebase.auth.UserCredential>
  ) => {
    e.preventDefault();
    try {
      setError("");
      await signInFunction();
      history.push("/");
    } catch (err) {
      setError(err.message);
    }
  };

  return (
    <button
      type="button"
      className={`btn border border-secondary mb-3 ${styles.signInButton}`}
      onClick={(e) => handleSigninWithProvider(e, handleSignIn)}
    >
      <img src={logo} alt={provider} className={styles.signInLogo} />
      Sign in with {provider}
    </button>
  );
}
