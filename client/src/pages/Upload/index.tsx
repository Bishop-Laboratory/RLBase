import React from "react";
import { useForm } from "react-hook-form";

const Upload = () => {
  const { register, handleSubmit/*, formState: { errors }*/ } = useForm();
  const onSubmit = (data:any) => console.log(data);

  return (
    <div className="bg-light p-5 rounded-lg m-3">
      <h1 className="display-4">Upload</h1>
      <form onSubmit={handleSubmit(onSubmit)}>
        <div className="form-group d-flex flex-column">
          <div className="input-group mb-3">
            <div className="input-group-prepend">
              <span className="input-group-text" id="basic-addon3">
                Title (optional)
              </span>
            </div>
            <input
              type="text"
              className="form-control"
              placeholder="..."
              aria-label="Username"
              aria-describedby="basic-addon1"
            />
          </div>
          <div className="input-group mb-3">
            <div className="input-group-prepend">
              <span className="input-group-text" id="basic-addon3">
                Email address
              </span>
            </div>
            <input
              type="email"
              className="form-control"
              placeholder="..."
              aria-label="email address"
              {...register("email", { required: true })}
            />
          </div>
          <div
            className="input-group mb-3 border d-flex align-items-center "
            style={{ width: 568 }}
          >
            <input
              type="file"
              accept=".xls,.narrowPeak,.broadPeak"
              className="form-control-file bg-white"
              style={{ width: 500 }}
              id="peaks"
              {...register("peaks", { required: true })}
            />
            <div className="input-group-append">
              <span className="input-group-text" id="basic-addon2">
                Peaks
              </span>
            </div>
          </div>
          <div
            className="input-group mb-3 border d-flex align-items-center "
            style={{ width: 711 }}
          >
            <input
              type="file"
              accept=".bw"
              className="form-control-file bg-white"
              style={{ width: 500 }}
              id="coverage"
              {...register("coverage", { required: true })}
            />
            <div className="input-group-append">
              <span className="input-group-text" id="basic-addon2">
                Coverage (bigWig format)
              </span>
            </div>
          </div>
          <div
            className="input-group mb-3 border d-flex align-items-center "
            style={{ width: 648 }}
          >
            <input
              type="file"
              accept=".txt"
              className="form-control-file bg-white"
              style={{ width: 500 }}
              id="expression"
              {...register("expression", { required: true })}
            />
            <div className="input-group-append">
              <span className="input-group-text" id="basic-addon2">
                Expression (TPM)
              </span>
            </div>
          </div>
          <div className="input-group mb-3">
            <div className="input-group-prepend">
              <label className="input-group-text" htmlFor="inputGroupSelect01">
                Type of dataset supplied
              </label>
            </div>
            <select className="custom-select" id="inputGroupSelect01">
              <option selected>Choose...</option>
              {[
                "DRIP-Seq",
                "qDRIP-Seq",
                "sDRIP-Seq",
                "DRIPc-Seq",
                " R-ChIP-Seq",
                "MapR",
              ].map((option) => (
                <option key={option} value={option}>
                  {option}
                </option>
              ))}
            </select>
          </div>
          <div className="input-group mb-3">
            <div className="input-group-prepend">
              <label className="input-group-text" htmlFor="inputGroupSelect01">
                Specie
              </label>
            </div>
            <select className="custom-select" id="inputGroupSelect02">
              <option selected>Choose...</option>
              {[
                "hg19", "hg38", "mm9", "mm10"
              ].map((option) => (
                <option key={option} value={option}>
                  {option}
                </option>
              ))}
            </select>
          </div>
          <input type="submit" value="Submit" />
        </div>
      </form>
    </div>
  );
};

export default Upload;
