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
      className="prefix__bi prefix__bi-arrow-up-short"
      viewBox="0 0 16 16"
      ref={svgRef}
      {...props}
    >
      <path
        fillRule="evenodd"
        d="M8 12a.5.5 0 00.5-.5V5.707l2.146 2.147a.5.5 0 00.708-.708l-3-3a.5.5 0 00-.708 0l-3 3a.5.5 0 10.708.708L7.5 5.707V11.5a.5.5 0 00.5.5z"
      />
    </svg>
  )
}

const ForwardRef = React.forwardRef(SvgComponent)
const ArrowUpShort = React.memo(ForwardRef)
export default ArrowUpShort
