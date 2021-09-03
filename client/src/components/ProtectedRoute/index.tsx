import React from "react";
import { Route, Redirect } from "react-router-dom";
import { useAuth } from "../../context/AuthContext";

export default function ProtectedRoute({
  path,
  component: Component,
}: {
  path: string;
  component: React.FC;
}): JSX.Element {
  const { currentUser } = useAuth();

  return <Route path={path}>{!currentUser ? <Redirect to="/" /> : <Component />}</Route>;
}
