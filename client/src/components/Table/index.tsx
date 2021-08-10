import React, { SetStateAction } from "react";
import { usePagination, useTable } from "react-table";
import { barChartDataItem } from "../../models";

const Table = ({
  data,
  setSelectedItem,
}: {
  data: any[];
  setSelectedItem: React.Dispatch<SetStateAction<barChartDataItem[]>>;
}) => {
  const columns = React.useMemo(
    () => [
      {
        Header: "ID",
        accessor: "id", // accessor is the "key" in the data
      },
      {
        Header: "Chr",
        accessor: "chr",
      },
      {
        Header: "start",
        accessor: "start",
      },
      {
        Header: "end",
        accessor: "end",
      },
      {
        Header: "type",
        accessor: "type",
      },
    ],
    []
  );
  const tableInstance = useTable(
    // @ts-ignore
    { columns, data, initialState: { pageIndex: 0 } },
    usePagination
  );
  const {
    getTableProps,
    getTableBodyProps,
    headerGroups,
    // @ts-ignore
    page,
    // @ts-ignore
    canPreviousPage,
    // @ts-ignore
    canNextPage,
    // @ts-ignore
    pageOptions,
    // @ts-ignore
    pageCount,
    // @ts-ignore
    gotoPage,
    // @ts-ignore
    nextPage,
    // @ts-ignore
    previousPage,
    // @ts-ignore
    setPageSize,
    // @ts-ignore

    state: { pageIndex, pageSize },
    prepareRow,
  } = tableInstance;
  return (
    <div className="ms-5">
      <table className=" table table-hover" {...getTableProps()}>
        <thead>
          {
            // Loop over the header rows
            headerGroups.map((headerGroup) => (
              // Apply the header row props
              <tr {...headerGroup.getHeaderGroupProps()}>
                {
                  // Loop over the headers in each row
                  headerGroup.headers.map((column) => (
                    // Apply the header cell props
                    <th {...column.getHeaderProps()}>
                      {
                        // Render the header
                        column.render("Header")
                      }
                    </th>
                  ))
                }
              </tr>
            ))
          }
        </thead>
        {/* Apply the table body props */}
        <tbody {...getTableBodyProps()}>
          {
            // Loop over the table rows
            page.map((row: any) => {
              // Prepare the row for display
              prepareRow(row);
              return (
                // Apply the row props
                <tr style={{ cursor: "pointer" }} onClick={() => null} {...row.getRowProps()}>
                  {
                    // Loop over the rows cells
                    row.cells.map((cell: any) => {
                      // Apply the cell props
                      return (
                        <td {...cell.getCellProps()}>
                          {
                            // Render the cell contents
                            cell.render("Cell")
                          }
                        </td>
                      );
                    })
                  }
                </tr>
              );
            })
          }
        </tbody>
      </table>
      <div className="d-flex w-100 justify-content-center">
        <ul className="pagination">
          <li className="page-item">
            <button
              className="page-link"
              onClick={() => gotoPage(0)}
              disabled={!canPreviousPage}
              aria-label="Previous"
            >
              &laquo;
            </button>
          </li>
          <li className="page-item">
            <button
              className="page-link"
              onClick={() => previousPage()}
              disabled={!canPreviousPage}
              aria-label="Previous"
            >
              &lt;
            </button>
          </li>
        </ul>
        <span className="ms-2 me-2 mt-2">
          Page{" "}
          <strong>
            {pageIndex + 1} of {pageOptions.length}
          </strong>{" "}
        </span>
        <ul className="pagination">
          <li className="page-item">
            <button
              className="page-link"
              onClick={() => nextPage()}
              disabled={!canNextPage}
              aria-label="Previous"
            >
              {">"}
            </button>
          </li>
          <li className="page-item">
            <button
              className="page-link"
              onClick={() => gotoPage(pageCount - 1)}
              disabled={!canNextPage}
              aria-label="Previous"
            >
              &raquo;
            </button>
          </li>
        </ul>
      </div>
      <div className="d-flex w-100 justify-content-center">
        <span>
          Go to page:{" "}
          <input
            type="number"
            defaultValue={pageIndex + 1}
            onChange={(e) => {
              const page = e.target.value ? Number(e.target.value) - 1 : 0;
              gotoPage(page);
            }}
            style={{ width: "100px" }}
          />
        </span>{" "}
        <select
          value={pageSize}
          onChange={(e) => {
            setPageSize(Number(e.target.value));
          }}
        >
          {[10, 20, 30, 40, 50].map((pageSize) => (
            <option key={pageSize} value={pageSize}>
              Show {pageSize}
            </option>
          ))}
        </select>
      </div>
    </div>
  );
};

export default Table;
