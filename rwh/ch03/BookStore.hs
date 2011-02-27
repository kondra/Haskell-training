data BookInfo = Book Int String [String]
                deriving (Show)

data MagazineInfo = Magazine Int String [String]
                    deriving (Show)

type CustomerID = Int
type ReviewBody = String

data BookReview = BookReview BookInfo Int String
data BetterReview = BetterReview BookInfo CustomerID ReviewBody

type CardHolder = String
type CardNumber = Int
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
				 | Invoice CustomerID
				   deriving (Show)

nicerID      (Book id _     _      ) = id
nicerTitle   (Book _  title _      ) = title
nicerAuthors (Book _  _     authors) = authors

data Customer = Customer {
              customerID      :: CustomerID,
			  customerName    :: String,
			  customerAddress :: Address
			  } deriving (Show)
