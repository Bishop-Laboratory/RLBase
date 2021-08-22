import React, { useContext, useState } from "react";

// TODO: set types

interface Alert {
  message: string;
  type: "success" | "danger" | "info"; // bootstrap alert types
}
interface AlertContextInterface {
  alert: Alert | null;
  setAlert: any;
}

const AlertContext = React.createContext<AlertContextInterface>({} as AlertContextInterface);

export function useAlert() {
  return useContext(AlertContext);
}

export function AlertProvider({ children }: { children: JSX.Element }): JSX.Element {
  const [alert, setAlert] = useState<Alert | null>(null);

  const value: AlertContextInterface = {
    alert,
    setAlert,
  };

  return <AlertContext.Provider value={value}>{children}</AlertContext.Provider>;
}
