import * as React from "react"

function SvgComponent(
  props: React.SVGProps<SVGSVGElement>,
  svgRef?: React.Ref<SVGSVGElement>
) {
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width={16}
      height={16}
      fill="currentColor"
      className="prefix__bi prefix__bi-arrow-down-short"
      viewBox="0 0 16 16"
      ref={svgRef}
      {...props}
    >
      <path
        fillRule="evenodd"
        d="M8 4a.5.5 0 01.5.5v5.793l2.146-2.147a.5.5 0 01.708.708l-3 3a.5.5 0 01-.708 0l-3-3a.5.5 0 11.708-.708L7.5 10.293V4.5A.5.5 0 018 4z"
      />
    </svg>
  )
}

const ForwardRef = React.forwardRef(SvgComponent)
const ArrowDownShort = React.memo(ForwardRef)
export default ArrowDownShort
