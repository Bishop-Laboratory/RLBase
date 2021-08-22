/* eslint-disable @typescript-eslint/no-explicit-any */
import React, { useContext, useState, useEffect } from "react";
import firebase from "firebase";
import { auth } from "../firebase";

// TODO: add types, remove 'any's

interface AuthContextInterface {
  currentUser: firebase.User | null;
  signup: any;
  sendEmailVerification: any;
  login: any;
  logout: any;
}

const AuthContext = React.createContext<AuthContextInterface>({} as AuthContextInterface);

export function useAuth() {
  return useContext(AuthContext);
}

export function AuthProvider({ children }: { children: JSX.Element }) {
  const [currentUser, setCurrentUser] = useState<firebase.User | null>(null);
  const [loading, setLoading] = useState<boolean>(true);

  function signup(email: string, password: string): Promise<firebase.auth.UserCredential> {
    return auth.createUserWithEmailAndPassword(email, password);
  }

  function sendEmailVerification(user: firebase.User) {
    return user.sendEmailVerification({ url: "http://localhost:3000" });
  }

  function login(email: string, password: string): Promise<firebase.auth.UserCredential> {
    return auth.signInWithEmailAndPassword(email, password);
  }

  function logout(): Promise<void> {
    return auth.signOut();
  }

  // attach an observer for sign-in state
  useEffect(() => {
    const unsubscribe = auth.onAuthStateChanged((user) => {
      setCurrentUser(user);
      setLoading(false);
    });

    // unsubscribe from the observer when component is unmounted
    return unsubscribe;
  }, []);

  const value: AuthContextInterface = {
    currentUser,
    signup,
    sendEmailVerification,
    login,
    logout,
  };

  return <AuthContext.Provider value={value}>{!loading && children}</AuthContext.Provider>;
}
