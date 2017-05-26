USE [LEI]
GO

/****** Object:  Table [dbo].[FactTable]    Script Date: 12/03/2017 15:24:25 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[FactTable](
	[id] [int] IDENTITY(1,1) NOT NULL,
	[IdCliente] [int] NOT NULL,
	[IdDataOrdem] [int] NOT NULL,
	[IdPais] [int] NOT NULL,
	[IdCartao] [int] NOT NULL,
	[Quantidade] [int] NOT NULL,
	[UnitPrice] [money] NOT NULL,
	[UnitPriceDiscount] [money] NOT NULL,
	[LineTotal] [numeric](36, 6) NOT NULL,
 CONSTRAINT [PK_FactTable] PRIMARY KEY CLUSTERED 
(
	[id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]

GO

ALTER TABLE [dbo].[FactTable]  WITH CHECK ADD  CONSTRAINT [FK_FactTable_CartaoCredito] FOREIGN KEY([IdCartao])
REFERENCES [dbo].[CartaoCredito] ([id])
GO

ALTER TABLE [dbo].[FactTable] CHECK CONSTRAINT [FK_FactTable_CartaoCredito]
GO

ALTER TABLE [dbo].[FactTable]  WITH CHECK ADD  CONSTRAINT [FK_FactTable_Cliente] FOREIGN KEY([IdCliente])
REFERENCES [dbo].[Cliente] ([id])
GO

ALTER TABLE [dbo].[FactTable] CHECK CONSTRAINT [FK_FactTable_Cliente]
GO

ALTER TABLE [dbo].[FactTable]  WITH CHECK ADD  CONSTRAINT [FK_FactTable_DataOrdem] FOREIGN KEY([IdDataOrdem])
REFERENCES [dbo].[DataOrdem] ([id])
GO

ALTER TABLE [dbo].[FactTable] CHECK CONSTRAINT [FK_FactTable_DataOrdem]
GO

ALTER TABLE [dbo].[FactTable]  WITH CHECK ADD  CONSTRAINT [FK_FactTable_Pais] FOREIGN KEY([IdPais])
REFERENCES [dbo].[Pais] ([id])
GO

ALTER TABLE [dbo].[FactTable] CHECK CONSTRAINT [FK_FactTable_Pais]
GO


