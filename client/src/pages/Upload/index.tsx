import React, { useState } from "react";
import styles from "./index.module.css";

interface FormData {
  sampleId: string;
  sampleCondition: string;
  sampleType: string;
  controlSample: string;
  group: string;
  submitterName: string;
  submitterEmail: string;
  uploadKey: string;
  csv: File | null;
}

export default function Upload(): JSX.Element {
  const [formData, setFormData] = useState<FormData>({
    sampleId: "",
    sampleCondition: "",
    sampleType: "",
    controlSample: "",
    group: "",
    submitterName: "",
    submitterEmail: "",
    uploadKey: "",
    csv: null,
  });

  const handleInputChange = (e: React.FormEvent<HTMLInputElement>) => {
    e.preventDefault();
    setFormData({
      ...formData,
      [e.currentTarget.id]: e.currentTarget.value,
    });
  };

  const handleFileChange = (e: React.FormEvent<HTMLInputElement>) => {
    e.preventDefault();
    if (e.currentTarget.files) {
      setFormData({
        ...formData,
        csv: e.currentTarget.files[0],
      });
    }
  };

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    // TODO: add post request to api-v1/rseq-worker
  };

  return (
    <main className="container">
      <h1 className="my-4 text-center h2">Upload a New Sample</h1>
      <form
        className={`${styles["upload-form"]} d-flex flex-column align-items-center`}
        onSubmit={handleSubmit}
      >
        <div className="row w-100">
          <h2 className="h4">Sample Information</h2>
        </div>
        <div className="row w-100 mb-3">
          <div className="col-md">
            <label htmlFor="sampleId" className="form-label">
              Sample ID
            </label>
            <input
              type="text"
              className="form-control"
              id="sampleId"
              aria-describedby="sampleId"
              onChange={handleInputChange}
              required
            />
          </div>
          <div className="col-md">
            <label htmlFor="sampleCondition" className="form-label">
              Sample Condition
            </label>
            <input
              type="text"
              className="form-control"
              id="sampleCondition"
              aria-describedby="sampleCondition"
              onChange={handleInputChange}
              required
            />
          </div>
          <div className="col-md">
            <label htmlFor="sampleType" className="form-label">
              Sample Type
            </label>
            <input
              type="text"
              className="form-control"
              id="sampleType"
              aria-describedby="sampleType"
              onChange={handleInputChange}
              required
            />
          </div>
        </div>
        <div className="row w-100 mb-3">
          <div className="col-md">
            <label htmlFor="controlSample" className="form-label">
              Control Sample
            </label>
            <input
              type="text"
              className="form-control"
              id="controlSample"
              aria-describedby="controlSample"
              onChange={handleInputChange}
              required
            />
          </div>
          <div className="col-md">
            <label htmlFor="csv" className="form-label">
              CSV file
            </label>
            <input
              className="form-control"
              type="file"
              id="csv"
              onChange={handleFileChange}
              required
            />
          </div>
        </div>
        <div className="row w-100 mt-3">
          <h2 className="h4">Submitter Information</h2>
        </div>
        <div className="row w-100 mb-3">
          <div className="col-md">
            <label htmlFor="submitterName" className="form-label">
              Name
            </label>
            <input
              type="text"
              className="form-control"
              id="submitterName"
              aria-describedby="submitterName"
              onChange={handleInputChange}
              required
            />
          </div>
          <div className="col-md">
            <label htmlFor="submitterEmail" className="form-label">
              Email
            </label>
            <input
              type="email"
              className="form-control"
              id="submitterEmail"
              aria-describedby="submitterEmail"
              onChange={handleInputChange}
              required
            />
          </div>
        </div>
        <div className="row w-100 mb-3">
          <div className="col-md">
            <label htmlFor="group" className="form-label">
              Group
            </label>
            <input
              type="text"
              className="form-control"
              id="group"
              aria-describedby="group"
              onChange={handleInputChange}
              required
            />
          </div>
          <div className="col-md">
            <label htmlFor="uploadKey" className="form-label">
              Upload Key
            </label>
            <input
              type="text"
              className="form-control"
              id="uploadKey"
              aria-describedby="uploadKey"
              onChange={handleInputChange}
              required
            />
          </div>
        </div>
        <button type="submit" className="btn btn-primary my-3">
          Submit
        </button>
      </form>
    </main>
  );
}
