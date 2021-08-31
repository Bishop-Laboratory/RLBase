import React, { useContext, useState, useEffect } from "react";
import firebase from "firebase";
import { auth, googleAuthProvider, githubAuthProvider } from "../firebase";

interface AuthContextInterface {
  currentUser: firebase.User | null;
  signup: (email: string, password: string) => Promise<firebase.auth.UserCredential>;
  sendEmailVerification: (user: firebase.User) => Promise<void>;
  loginWithEmailAndPAssword: (
    email: string,
    password: string
  ) => Promise<firebase.auth.UserCredential>;
  signInWithGoogle: () => Promise<firebase.auth.UserCredential>;
  signInWithGithub: () => Promise<firebase.auth.UserCredential>;
  isVerified: () => boolean;
  logout: () => Promise<void>;
}

const AuthContext = React.createContext<AuthContextInterface>({} as AuthContextInterface);

export function useAuth(): AuthContextInterface {
  return useContext(AuthContext);
}

export function AuthProvider({ children }: { children: JSX.Element }) {
  const [currentUser, setCurrentUser] = useState<firebase.User | null>(null);
  const [loading, setLoading] = useState<boolean>(true);

  function signup(email: string, password: string): Promise<firebase.auth.UserCredential> {
    return auth.createUserWithEmailAndPassword(email, password);
  }

  function sendEmailVerification(user: firebase.User): Promise<void> {
    return user.sendEmailVerification({ url: "http://localhost:3000" });
  }

  function loginWithEmailAndPAssword(
    email: string,
    password: string
  ): Promise<firebase.auth.UserCredential> {
    return auth.signInWithEmailAndPassword(email, password);
  }

  async function signInWithGoogle(): Promise<firebase.auth.UserCredential> {
    return auth.signInWithPopup(googleAuthProvider);
  }

  function signInWithGithub(): Promise<firebase.auth.UserCredential> {
    return auth.signInWithPopup(githubAuthProvider);
  }

  function logout(): Promise<void> {
    return auth.signOut();
  }

  function isVerified(): boolean {
    if (currentUser) {
      // Only password auth needs email verification
      if (currentUser.providerData[0] && currentUser.providerData[0].providerId === "password") {
        return currentUser.emailVerified;
      }
      return true;
    }
    return false;
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
    loginWithEmailAndPAssword,
    signInWithGoogle,
    signInWithGithub,
    isVerified,
    logout,
  };

  return <AuthContext.Provider value={value}>{!loading && children}</AuthContext.Provider>;
}
