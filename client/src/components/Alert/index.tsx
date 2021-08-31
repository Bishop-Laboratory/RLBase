import React, { useEffect } from "react";
import { useAlert } from "../../context/AlertContext";
import { useAuth } from "../../context/AuthContext";

export default function Alert() {
  const { alert, setAlert } = useAlert();
  const { currentUser, isVerified } = useAuth();

  useEffect(() => {
    if (currentUser && !isVerified()) {
      setAlert({
        type: "info",
        message: "Your account is unverified. Check your email for a verification link.",
      });
    } else {
      setAlert(null);
    }
  }, [currentUser]);

  if (alert) {
    return (
      <div className={`container mt-2 alert alert-${alert.type}`} role="alert">
        <p className="m-0 text-center">{alert.message}</p>
      </div>
    );
  }

  return null;
}
