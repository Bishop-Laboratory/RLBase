/* eslint-disable react/button-has-type */
/* eslint-disable no-nested-ternary */
/* eslint-disable react/no-array-index-key */
/* eslint-disable no-shadow */
/* eslint-disable @typescript-eslint/ban-ts-comment */
/* eslint-disable @typescript-eslint/no-use-before-define */
import React from "react";
import { useHistory } from "react-router-dom";
import { useFilters, usePagination, useSortBy, useTable } from "react-table";
import ArrowDownShort from "../../svg/icons/ArrowDownShort";
import ArrowUpShort from "../../svg/icons/ArrowUpShort";

const SearchTable = ({ data, match }: { data: any[]; match: any }) => {
  const { push } = useHistory();
  const columns = React.useMemo(() => {
    if (match.params.type === "r-loop") {
      return [
        {
          Header: "ID",
          accessor: "id", // accessor is the "key" in the data
        },
        {
          Header: "Chr",
          accessor: "Chr",
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
      ];
    }
    return [
      {
        Header: "R-Loop",
        accessor: "SRX", // accessor is the "key" in the data
      },
      {
        Header: "Cell Type",
        accessor: "Cell",
        Filter: SelectColumnFilter,
      },
      {
        Header: "Info",
        accessor: "mode",
      },
      {
        Header: "Genotype",
        accessor: "Genotype",
        Filter: SelectColumnFilter,
      },
      {
        Header: "Study",
        accessor: "Group",
        Filter: SelectColumnFilter,
      },
      {
        Header: "Specie",
        accessor: "Species",
        Filter: SelectColumnFilter,
      },
    ];
  }, [match.params.type]);
  // Define a default UI for filtering
  function DefaultColumnFilter({ column: { filterValue, preFilteredRows, setFilter } }: any) {
    const count = preFilteredRows.length;

    return (
      <input
        value={filterValue || ""}
        onChange={(e) => {
          setFilter(e.target.value || undefined); // Set undefined to remove the filter entirely
        }}
        placeholder={`Search ${count} records...`}
      />
    );
  }
  const defaultColumn = React.useMemo(
    () => ({
      // Let's set up our default Filter UI
      Filter: DefaultColumnFilter,
    }),
    []
  );
  function SelectColumnFilter({ column: { filterValue, setFilter, preFilteredRows, id } }: any) {
    // Calculate the options for filtering
    // using the preFilteredRows
    const options = React.useMemo(() => {
      const options = new Set();
      preFilteredRows.forEach((row: any) => {
        options.add(row.values[id]);
      });
      // @ts-ignore
      return [...options.values()];
    }, [id, preFilteredRows]);

    // Render a multi-select box
    return (
      <select
        value={filterValue}
        style={{ width: 150 }}
        onChange={(e) => {
          setFilter(e.target.value || undefined);
        }}
      >
        <option value="">All</option>
        {options.map((option, i) => (
          <option key={i} value={option}>
            {option}
          </option>
        ))}
      </select>
    );
  }

  const onRowClick = (row: any) => {
    switch (match.params.type) {
      case "r-loop":
        return push(`/about?rloop=${row.original.rloopId}`);
      case "sample":
        return push(`/explorer?GSM=${row.original.GSM}`);
      case "gene":
        return push(`/samples?gene=${row.original.geneId}`);
      default:
        return push("/");
    }
  };

  const tableInstance = useTable(
    // @ts-ignore
    { columns, data, initialState: { pageIndex: 0 }, defaultColumn },
    useFilters,
    useSortBy,
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
    <div className="ms-5" style={{ width: 800 }}>
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
                    <th // @ts-ignore
                      {...column.getHeaderProps(column.getSortByToggleProps())}
                    >
                      {
                        // Render the header
                        column.render("Header")
                      }
                      {/* @ts-ignore */}
                      {column.isSorted ? (
                        // @ts-ignore
                        column.isSortedDesc ? (
                          <ArrowDownShort />
                        ) : (
                          <ArrowUpShort />
                        )
                      ) : (
                        ""
                      )}
                      {/* @ts-ignore */}
                      <div>{column.canFilter ? column.render("Filter") : null}</div>
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
                <tr
                  onClick={() => onRowClick(row)}
                  style={{ cursor: "pointer" }}
                  {...row.getRowProps()}
                >
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

export default SearchTable;
