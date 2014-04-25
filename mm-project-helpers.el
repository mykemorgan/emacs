;; $Id: mm-project-helpers.el,v 1.1 2005/09/14 19:11:40 mimorgan Exp $

(defun mm-load-api-files ()
  "Load a sampling of API related files into buffers"
  (interactive)
  (save-excursion
;    (find-file (concat (getenv "BUILDTOP") "/package/server/nvppaymentserv/nvppaymentserv.pkg_def"))
;    (find-file (concat (getenv "BUILDTOP") "/package/server/nvppaymentserv/nvppaymentserv.cpp"))
;    (find-file (concat (getenv "BUILDTOP") "/biz/Merchant/Payment/Common/Session/implementation/CheckoutSessionImpl.h"))
    (find-file (concat (getenv "BUILDTOP") "/biz/Merchant/Payment/Common/Session/data_access/CheckoutVO.oml"))
    (find-file (concat (getenv "BUILDTOP") "/biz/Merchant/Payment/service/NVPPaymentServ.sdl"))
;    (find-file (concat (getenv "BUILDTOP") "/biz/Merchant/API/NVPServerMain.h"))
    (find-file (concat (getenv "BUILDTOP") "/biz/Merchant/API/value_object/NVPCommonParams.oml"))
;    (find-file (concat (getenv "BUILDTOP") "/applogic/server/Merchant/Payment/API/nvppaymentserv/common/api_nvppaymentserv_common.lib_def"))
;    (find-file (concat (getenv "BUILDTOP") "/applogic/server/Merchant/Payment/API/nvppaymentserv/utility/api_nvppaymentserv_utility.lib_def"))
;    (find-file (concat (getenv "BUILDTOP") "/applogic/server/Merchant/Payment/API/nvppaymentserv/implementation/NVPPaymentServImpl.h"))
;    (find-file (concat (getenv "BUILDTOP") "/pimp/Merchant/Payment/ExpressCheckout/PimpWalletBLIImpl.cpp"))
;    (find-file (concat (getenv "BUILDTOP") "/pimp/Merchant/Payment/ExpressCheckout/PimpWalletBLI.h"))
    (find-file (concat (getenv "BUILDTOP") "/ppapi_v2/wsdl/Common/eBLBaseComponents.xsd"))
    (find-file (concat (getenv "BUILDTOP") "/ppapi_v2/ebl.soap_inc"))
;    (find-file (concat (getenv "BUILDTOP") "/biz/Merchant/Payment/CheckoutOperationFactory.cpp"))
;    (find-file (concat (getenv "BUILDTOP") "/biz/Merchant/API/OperationBase.cpp"))
;    (find-file (concat (getenv "BUILDTOP") "/applogic/server/Merchant/Payment/ExpressCheckout/SetEC/SetExpressCheckout.h"))
;    (find-file (concat (getenv "BUILDTOP") "/applogic/server/Merchant/Payment/ExpressCheckout/SetEC/SetExpressCheckoutSoap.cpp"))
;    (find-file (concat (getenv "BUILDTOP") "/applogic/server/Merchant/Payment/ExpressCheckout/SetEC/SetExpressCheckout.cpp"))

;    (find-file (concat (getenv "BUILDTOP") ""))
  )
)


